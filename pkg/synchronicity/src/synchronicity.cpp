#include <iostream>
#include <boost/interprocess/sync/named_upgradable_mutex.hpp>
#include <boost/bind.hpp>

#include <R.h>
#include <Rdefines.h>

#include "util.h"
#include "SharedCounter.h"

using namespace boost;
using namespace boost::interprocess;
using namespace boost::posix_time;

// Use these functions for locking and unlock.

// Note, we don't actually create the mutexes until the first time
// it is locked.

template<typename LockFunctionType>
SEXP boost_lock(SEXP resourceName, LockFunctionType lockFun)
{
  named_upgradable_mutex mutex(open_or_create, CHARACTER_VALUE(resourceName));
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  try
  {
    lockFun(mutex);
    LOGICAL_DATA(ret)[0] = Rboolean(1);
  }
  catch (std::exception &e)
  {
    LOGICAL_DATA(ret)[0] = Rboolean(0);
    printf("%s\n", e.what());
  }
  UNPROTECT(1);
  return ret;
}

template<typename TryLockFunctionType>
SEXP boost_try_lock(SEXP resourceName, TryLockFunctionType tryLockFun)
{
  named_upgradable_mutex mutex(open_or_create, CHARACTER_VALUE(resourceName));
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  try
  {
    LOGICAL_DATA(ret)[0] = tryLockFun(mutex) ? Rboolean(1) : Rboolean(0);
  }
  catch (std::exception &e)
  {
    LOGICAL_DATA(ret)[0] = Rboolean(0);
    printf("%s\n", e.what());
  }
  UNPROTECT(1);
  return ret;
}

template<typename UnlockFunctionType>
SEXP boost_unlock(SEXP resourceName, UnlockFunctionType unlockFun)
{
  named_upgradable_mutex mutex(open_or_create, CHARACTER_VALUE(resourceName));
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  try
  {
    unlockFun(mutex);
    LOGICAL_DATA(ret)[0] = Rboolean(1);
  }
  catch (std::exception &e)
  {
    LOGICAL_DATA(ret)[0] = Rboolean(0);
    printf("%s\n", e.what());
  }
  UNPROTECT(1);
  return ret;
}

extern "C"
{

void DestroySharedCounter( SEXP sharedCounterAddr )
{
  SharedCounter *pSharedCounter = 
    reinterpret_cast<SharedCounter*>(R_ExternalPtrAddr(sharedCounterAddr));
  delete pSharedCounter;
  R_ClearExternalPtr(sharedCounterAddr);
}

SEXP CreateSharedCounter( SEXP resourceName )
{
  SharedCounter *pSharedCounter = new SharedCounter();
  pSharedCounter->init( RChar2String(resourceName)+"_counter" );
  SEXP address = R_MakeExternalPtr( pSharedCounter, R_NilValue, R_NilValue );
  R_RegisterCFinalizerEx( address, (R_CFinalizer_t)DestroySharedCounter,
    (Rboolean)TRUE );
  return address;
}

SEXP destroy_mutex( SEXP resourceName )
{
  SEXP ret = PROTECT(NEW_LOGICAL(1));
  try
  {
    named_upgradable_mutex::remove( CHARACTER_VALUE(resourceName) );
    LOGICAL_DATA(ret)[0] = Rboolean(1);
  }
  catch(std::exception &e)
  {
    LOGICAL_DATA(ret)[0] = Rboolean(0);
    printf("%s\n", e.what());
  }
  UNPROTECT(1);
  return ret;
}

SEXP boost_lock( SEXP resourceName )
{
  return boost_lock( resourceName, bind( &named_upgradable_mutex::lock, _1 ) );
}

SEXP boost_try_lock( SEXP resourceName )
{
  return boost_try_lock( resourceName, 
    bind( &named_upgradable_mutex::try_lock, _1 ) );
}

SEXP boost_unlock( SEXP resourceName )
{
  return boost_unlock( resourceName, 
    bind( &named_upgradable_mutex::unlock, _1 ) );
}

SEXP boost_lock_shared( SEXP resourceName )
{
  return boost_lock( resourceName, 
    bind( &named_upgradable_mutex::lock_sharable, _1 ) );
}

SEXP boost_try_lock_shared( SEXP resourceName )
{
  return boost_lock( resourceName, 
    bind( &named_upgradable_mutex::try_lock_sharable, _1 ) );
}

SEXP boost_unlock_shared( SEXP resourceName )
{
  return boost_unlock( resourceName,
    bind( &named_upgradable_mutex::unlock_sharable, _1 ) );
}

ptime to_ptime( SEXP timeout )
{
  return second_clock::local_time() + 
    seconds( static_cast<long>(NUMERIC_VALUE(timeout)) );
}

SEXP boost_lock_timed( SEXP resourceName, SEXP timeout )
{
  return boost_lock( resourceName,
    bind( &named_upgradable_mutex::timed_lock, _1, to_ptime(timeout) ) );
}

SEXP boost_unlock_timed( SEXP resourceName, SEXP timeout )
{
  return boost_unlock( resourceName,
    bind( &named_upgradable_mutex::unlock, _1 ) );
}

SEXP boost_lock_shared_timed( SEXP resourceName, SEXP timeout )
{
  return boost_unlock( resourceName,
    bind(&named_upgradable_mutex::timed_lock_sharable, _1, to_ptime(timeout)) );
}

SEXP boost_unlock_shared_timed( SEXP resourceName, SEXP timeout )
{
  return boost_lock( resourceName, 
    bind( &named_upgradable_mutex::unlock, _1 ) );
}

}
