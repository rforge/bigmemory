#include <iostream>
#include <string>
#include <boost/interprocess/sync/named_upgradable_mutex.hpp>
#include <boost/interprocess/sync/named_mutex.hpp>
#include <boost/bind.hpp>

#include <R.h>
#include <Rdefines.h>

#include "util.h"
#include "SharedCounter.h"

using namespace std;
using namespace boost;
using namespace boost::interprocess;
using namespace boost::posix_time;

class BoostMutexInfo
{
  public:

    BoostMutexInfo() : 
      _name("") {}
    
    virtual ~BoostMutexInfo() {destroy();}
  

    bool init(const std::string &newName)
    {
      _name = newName;
      _counter.init(newName+"_counter");
    }

    // This function must be protected by a semaphore.
    bool destroy()
    {
      if (_counter.get() == 1)
      {
        try
        {
          named_upgradable_mutex::remove( _name.c_str() );
          return true;
        }
        catch(std::exception &e)
        {
          printf("%s\n", e.what());
          return false;
        }
      }
    }

    std::string name() const {return _name;}

    SharedCounter count() const {return _counter;}

  protected:
    std::string _name;
    SharedCounter _counter;
    
};
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

void DestroyBoostMutexInfo( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pbmi = 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  std::string cmName = pbmi->name()+"_counter_mutex";
  named_mutex mutex(open_or_create, cmName.c_str());
  delete pbmi;
  R_ClearExternalPtr(mutexInfoAddr);
  named_mutex::remove( cmName.c_str() );
}

SEXP CreateBoostMutexInfo( SEXP resourceName )
{
  BoostMutexInfo *pbmi = new BoostMutexInfo();
  pbmi->init( RChar2String(resourceName) );
  SEXP address = R_MakeExternalPtr( pbmi, R_NilValue, R_NilValue );
  R_RegisterCFinalizerEx( address, (R_CFinalizer_t)DestroyBoostMutexInfo,
    (Rboolean)TRUE );
  return(address);
}

SEXP GetResourceName( SEXP mutexInfoAddr )
{
  BoostMutexInfo *pbmi = 
    reinterpret_cast<BoostMutexInfo*>(R_ExternalPtrAddr(mutexInfoAddr));
  return String2RChar( pbmi->name() );
}

/*
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
*/
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