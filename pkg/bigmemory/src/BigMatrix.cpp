#include <exception>
#include <string>
#include <sstream>
#include <fstream>
#include <fcntl.h> // to remove files
#include <unistd.h> // to truncate files
#include <errno.h>

#include <boost/interprocess/shared_memory_object.hpp>
#include <boost/interprocess/file_mapping.hpp>
#include <boost/interprocess/mapped_region.hpp>
#include <boost/interprocess/sync/scoped_lock.hpp>
#include <boost/uuid.hpp>
#include <boost/exception/exception.hpp>
#include <boost/interprocess/sync/named_mutex.hpp>

#include "bigmemory/BigMatrix.h"

using namespace std;
using namespace boost;
using namespace boost::interprocess;

template<typename T>
std::string ttos(T i)
{
  stringstream s;
  s << i;
  return s.str();
}

bool SharedBigMatrix::create_uuid()
{
  try
  {
    // See http://www.boost.org/doc/libs/1_36_0/libs/random/random_demo.cpp
    // for documentation about seed problems with random number based uuid.
    boost::uuid id = uuid::create();
    _uuid = id.to_string();
    return true;
  }
  catch(std::exception &e)
  {
    printf("%s\n", e.what());
    printf("%s line %d\n", __FILE__, __LINE__);
    return false;
  }
}

template<typename T>
void* CreateSharedSepMatrix( const std::string &sharedName, 
  MappedRegionPtrs &dataRegionPtrs, const index_type nrow, 
  const index_type ncol )
{
  T** pMat = new T*[ncol];
  index_type i;
  dataRegionPtrs.resize(ncol);
  for (i=0; i < ncol; ++i)
  {
    try
    {
//      shared_memory_object::remove( (sharedName+"_column_"+ttos(i)).c_str() );
      shared_memory_object shm(create_only, 
        (sharedName + "_column_" + ttos(i)).c_str(),
        read_write);
      shm.truncate( nrow*sizeof(T) );
      dataRegionPtrs[i] = MappedRegionPtr( new MappedRegion(shm, read_write) );
      pMat[i] = reinterpret_cast<T*>( dataRegionPtrs[i]->get_address());
    }
    catch (interprocess_exception &e)
    {
      printf("%s\n", e.what());
      printf("%s line %d\n", __FILE__, __LINE__);
      index_type j;
      for (j=0; j < i; ++j)
      {
        shared_memory_object::remove( (sharedName+"_column_"+ttos(j)).c_str());
      }
      delete pMat;
      return false;
    }
  }
  return reinterpret_cast<void*>(pMat);
}

template<typename T>
void* CreateSharedMatrix( const std::string &sharedName, 
  MappedRegionPtrs &dataRegionPtrs, const index_type nrow, const index_type ncol)
{
  try
  {
//    shared_memory_object::remove( (sharedName.c_str()) );
    shared_memory_object shm(create_only, sharedName.c_str(), read_write);
    shm.truncate( nrow*ncol*sizeof(T) );
    dataRegionPtrs.push_back(
      MappedRegionPtr(new MappedRegion(shm, read_write)));
  }
  catch (interprocess_exception &ex)
  {
    printf("%s\n", ex.what());
    printf("%s line %d\n", __FILE__, __LINE__);
    shared_memory_object::remove( (sharedName.c_str() ) );
    return NULL;
  }
  return dataRegionPtrs[0]->get_address();
}

bool SharedMemoryBigMatrix::create( const index_type numRow, 
  const index_type numCol, const int matrixType, 
  const bool sepCols )
{
  using namespace boost::interprocess;
  if (!create_uuid())
  {
    return false;
  }
  try
  {
    _nrow = numRow;
    _totalRows = _nrow;
    _ncol = numCol;
    _totalCols = _ncol;
    _matType = matrixType;
    _sepCols = sepCols;
    _sharedName=_uuid;
    // Create the associated mutex and counter;
    named_mutex mutex(open_or_create, (_sharedName+"_counter_mutex").c_str());
    mutex.lock();
    _counter.init( _sharedName+"_counter" );
    mutex.unlock();
    if (_sepCols)
    {
      switch(_matType)
      {
        case 1:
          _pdata = CreateSharedSepMatrix<char>(_sharedName, _dataRegionPtrs,
            _nrow, _ncol);
          break;
        case 2:
          _pdata = CreateSharedSepMatrix<short>(_sharedName, _dataRegionPtrs, 
            _nrow, _ncol);
          break;
        case 4:
          _pdata = CreateSharedSepMatrix<int>(_sharedName, _dataRegionPtrs, 
            _nrow, _ncol);
          break;
        case 8:
          _pdata = CreateSharedSepMatrix<double>(_sharedName, _dataRegionPtrs,
            _nrow, _ncol);
      }
    }
    else
    {
      switch(_matType)
      {
        case 1:
          _pdata = CreateSharedMatrix<char>(_sharedName, _dataRegionPtrs, 
            _nrow, _ncol);
          break;
        case 2:
          _pdata = CreateSharedMatrix<short>(_sharedName, _dataRegionPtrs,
            _nrow, _ncol);
          break;
        case 4:
          _pdata = CreateSharedMatrix<int>(_sharedName, _dataRegionPtrs,
            _nrow, _ncol);
          break;
        case 8:
          _pdata = CreateSharedMatrix<double>(_sharedName, _dataRegionPtrs,
            _nrow, _ncol);
      }
    }
    if (_pdata == NULL)
    {
      return false;
    }
    return true;
  }
  catch(std::exception &e)
  {
    printf("%s\n", e.what());
    printf("%s line %d\n", __FILE__, __LINE__);
    return false;
  }
}

template<typename T>
void* ConnectSharedSepMatrix( const std::string &uuid, 
  MappedRegionPtrs &dataRegionPtrs, const index_type nrow, 
  const index_type ncol )
{
  T** pMat = new T*[ncol];
  index_type i;
  for (i=0; i < ncol; ++i)
  {
    try
    {
      shared_memory_object shm(open_only,
        (uuid + "_column_" + ttos(i)).c_str(),
        read_write);
      dataRegionPtrs.push_back(
        MappedRegionPtr(new MappedRegion(shm, read_write)));
      pMat[i] = reinterpret_cast<T*>(dataRegionPtrs[i]->get_address());
    }
    catch (std::exception &e)
    {
      printf("%s\n", e.what());
      printf("%s line %d\n", __FILE__, __LINE__);
      dataRegionPtrs.resize(0);
      delete pMat;
      return NULL;
    }
  }
  return reinterpret_cast<void*>(pMat);
}

template<typename T>
void* ConnectSharedMatrix( const std::string &sharedName, 
  MappedRegionPtrs &dataRegionPtrs, SharedCounter &counter, 
  const index_type nrow, const index_type ncol)
{
  using namespace boost::interprocess;
  try 
  {
    shared_memory_object shm(open_only, sharedName.c_str(), read_write);
    dataRegionPtrs.push_back(
      MappedRegionPtr(new MappedRegion(shm, read_write)));
  }
  catch(std::exception &e)
  {
    printf("%s\n", e.what());
    printf("%s line %d\n", __FILE__, __LINE__);
    dataRegionPtrs.resize(0);
    return NULL;
  }
  return reinterpret_cast<void*>(dataRegionPtrs[0]->get_address());
}

bool SharedMemoryBigMatrix::connect( const std::string &uuid, 
  const index_type numRow, const index_type numCol, const int matrixType, 
  const bool sepCols )
{
  using namespace boost::interprocess;
  try
  {
    _uuid=uuid;
    _sharedName = _uuid;
    _nrow = numRow;
    _totalRows=_nrow;
    _ncol = numCol;
    _totalCols=_ncol;
    _matType = matrixType;
    _sepCols = sepCols;
    // Attach to the associated mutex and counter;
    named_mutex mutex(open_or_create, (_sharedName+"_counter_mutex").c_str());
    mutex.lock();
    _counter.init( _sharedName+"_counter" );
    mutex.unlock();
    if (_sepCols)
    {
      switch(_matType)
      {
// JJJ I don't think nrow is needed below.
        case 1:
          _pdata = ConnectSharedSepMatrix<char>(_sharedName, _dataRegionPtrs,
            _nrow, _ncol);
          break;
        case 2:
          _pdata = ConnectSharedSepMatrix<short>(_sharedName, _dataRegionPtrs,
            _nrow, _ncol);
          break;
        case 4:
          _pdata = ConnectSharedSepMatrix<int>(_sharedName, _dataRegionPtrs,
            _nrow, _ncol);
          break;
        case 8:
          _pdata = ConnectSharedSepMatrix<double>(_sharedName, _dataRegionPtrs,
            _nrow, _ncol);
      }
    }
    else
    {
      switch(_matType)
      {
// JJJ I don't think nrow and ncol are needed below.
        case 1:
          _pdata = ConnectSharedMatrix<char>(_sharedName, _dataRegionPtrs,
            _counter, _nrow, _ncol);
          break;
        case 2:
          _pdata = ConnectSharedMatrix<short>(_sharedName, _dataRegionPtrs,
            _counter, _nrow, _ncol);
          break;
        case 4:
          _pdata = ConnectSharedMatrix<int>(_sharedName, _dataRegionPtrs,
            _counter, _nrow, _ncol);
          break;
        case 8:
          _pdata = ConnectSharedMatrix<double>(_sharedName, _dataRegionPtrs,
            _counter, _nrow, _ncol);
      }
    }
    if (!_pdata)
    {
      return false;
    }
    return true;
  }
  catch(std::exception &e)
  {
    printf("%s\n", e.what());
    printf("%s line %d\n", __FILE__, __LINE__);
    return false;
  }
}

void DestroySharedSepMatrix( const std::string &uuid, const index_type ncol )
{
  index_type i;
  for (i=0; i < ncol; ++i)
  {
    try
    {
      shared_memory_object::remove((uuid+ "_column_" + ttos(i)).c_str());
    }
    catch(std::exception &e)
    {
      printf("%s\n", e.what());
      printf("%s line %d\n", __FILE__, __LINE__);
    }
  }
}

bool SharedMemoryBigMatrix::destroy()
{
  using namespace boost::interprocess;
  named_mutex mutex(open_or_create, (_sharedName+"_counter_mutex").c_str());
  mutex.lock();
  bool destroyThis = (1==_counter.get()) ? true : false;
  try
  {
    _dataRegionPtrs.resize(0);
    // If this is the last shared memory big matrix destroy it and the
    // associated mutex.  The counter will destroy itself.

    if (_sepCols)
    {
      if (destroyThis)
      {
        DestroySharedSepMatrix(_uuid, _totalCols);
      }
      if (_pdata)
      {
          delete [] reinterpret_cast<char**>(_pdata);
      }
    }
    else
    {
      if (destroyThis)
      {
        shared_memory_object::remove(_uuid.c_str());
      }
    }
    mutex.unlock();
    if (destroyThis)
    {
      named_mutex::remove((_sharedName+"_counter_mutex").c_str());
    }
    return true;
  }
  catch(std::exception &e)
  {
    printf("%s\n", e.what());
    printf("%s line %d\n", __FILE__, __LINE__);
    mutex.unlock();
    if (destroyThis)
    {
      named_mutex::remove((_sharedName+"_counter_mutex").c_str());
    }
    return false;
  }
}

template<typename T>
void* ConnectFileBackedSepMatrix( const std::string &sharedName,
  const std::string &filePath, MappedRegionPtrs &dataRegionPtrs, 
  const index_type nrow, const index_type ncol)
{
  T** pMat = new T*[ncol];
  index_type i;
  dataRegionPtrs.resize(ncol);
  for (i=0; i < ncol; ++i)
  {
    std::string columnName = filePath + sharedName + "_column_" + ttos(i);
    // Map the file to this process.
    try
    {
      file_mapping mFile(columnName.c_str(), read_write);
      dataRegionPtrs[i] = MappedRegionPtr(new MappedRegion(mFile, read_write));
      pMat[i] = reinterpret_cast<T*>(dataRegionPtrs[i]->get_address());
    }
    catch (std::exception &e)
    {
      printf("%s\n", e.what());
      printf("%s line %d\n", __FILE__, __LINE__);
      dataRegionPtrs.resize(0);
      delete [] pMat;
      return NULL;
    }
  }
  return reinterpret_cast<void*>(pMat);
}

template<typename T>
void* CreateFileBackedSepMatrix( const std::string &fileName, 
  const std::string &filePath, MappedRegionPtrs &dataRegionPtrs, 
  const index_type nrow, const index_type ncol )
{
#ifdef WINDOWS
  index_type i;
  for (i=0; i < ncol; ++i)
  {
    std::string columnName = filePath + fileName + "_column_" + ttos(i);
    // Create the files.
    std::filebuf fbuf;
    if (!fbuf.open( columnName.c_str(), std::ios_base::in | std::ios_base::out |
      std::ios_base::trunc | std::ios_base::binary ))
    {
      return NULL;
    }
    fbuf.pubseekoff( nrow*sizeof(T), std::ios_base::beg);
    // I'm not sure if I need this next line
    fbuf.sputc(0);
    fbuf.close();
  }
#else
  index_type i;
  for (i=0; i < ncol; ++i)
  {
    std::string columnName = filePath + fileName + "_column_" + ttos(i);
    FILE *fp = fopen( columnName.c_str(), "wb");
    if ( -1 == ftruncate( fileno(fp), nrow*sizeof(T) ) )
    {
      printf("Problem creating file %s.\n", columnName.c_str());
      index_type j;
      for (j=0; j < i; ++j)
      {
        columnName = filePath + fileName + "_column_" + ttos(j);
        unlink( columnName.c_str() );
        return NULL;
      }
    }
    fclose(fp);
  }
#endif
  return ConnectFileBackedSepMatrix<T>(fileName, filePath, dataRegionPtrs, 
    nrow, ncol);
}

template<typename T>
void* ConnectFileBackedMatrix( const std::string &fileName, 
  const std::string &filePath, MappedRegionPtrs &dataRegionPtrs, 
  const index_type nrow, const index_type ncol)
{
  try
  {
    file_mapping mFile((filePath+fileName).c_str(), read_write);
    dataRegionPtrs.push_back(
      MappedRegionPtr(new MappedRegion(mFile, read_write)));
  }
  catch (std::exception &e)
  {
    printf("%s\n", e.what());
    printf("%s line %d\n", __FILE__, __LINE__);
    dataRegionPtrs.resize(0);
    return NULL;
  }
  return reinterpret_cast<void*>(dataRegionPtrs[0]->get_address());
}

template<typename T>
void* CreateFileBackedMatrix(const std::string &fileName, 
  const std::string &filePath, MappedRegionPtrs &dataRegionPtrs, 
  const index_type nrow, const index_type ncol)
{
  // Create the file.
  std::string fullFileName = filePath+fileName;
#ifdef WINDOWS
  std::filebuf fbuf;
  if (!fbuf.open( (filePath+fileName).c_str(),
      std::ios_base::in | std::ios_base::out |
      std::ios_base::trunc | std::ios_base::binary ))
  {
    return NULL;
  }
  fbuf.pubseekoff( nebytes+nrow*ncol*sizeof(T), std::ios_base::beg);
  // I'm not sure if I need this next line
  fbuf.sputc(0);
  fbuf.close();
#else
  FILE *fp = fopen( fullFileName.c_str(), "wb");
  if (!fp)
  {
    printf( "Problem creating file %s.\n", fullFileName.c_str() );
    return NULL;
  }  
  if (-1 == ftruncate( fileno(fp), nrow*ncol*sizeof(T) ) )
  {
    printf( "Error: %s\n", strerror(errno) );
    fclose(fp);
    return NULL;
  }
  fclose(fp);
#endif
  return ConnectFileBackedMatrix<T>(fileName, filePath,
    dataRegionPtrs, nrow, ncol);
}

bool FileBackedBigMatrix::create( const std::string &fileName, 
  const std::string &filePath, const index_type numRow, const index_type numCol, 
  const int matrixType, const bool sepCols)
{
  if (!create_uuid())
  {
    return false;
  }
  try
  {
    _fileName = fileName;
    _sharedName=fileName+uuid();
    _nrow = numRow;
    _totalRows = _nrow;
    _ncol = numCol;
    _totalCols = _ncol;
    _matType = matrixType;
    _sepCols = sepCols;
    if (_sepCols)
    {
      switch(_matType)
      {
        case 1:
          _pdata = CreateFileBackedSepMatrix<char>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
          break;
        case 2:
          _pdata = CreateFileBackedSepMatrix<short>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
          break;
        case 4:
          _pdata = CreateFileBackedSepMatrix<int>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
          break;
        case 8:
          _pdata = CreateFileBackedSepMatrix<double>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
      }
    }
    else
    {
      switch(_matType)
      {
        case 1:
          _pdata = CreateFileBackedMatrix<char>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
          break;
        case 2:
          _pdata = CreateFileBackedMatrix<short>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
          break;
        case 4:
          _pdata = CreateFileBackedMatrix<int>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
          break;
        case 8:
          _pdata = CreateFileBackedMatrix<double>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
      }
    }
    if (!_pdata)
    {
      return false;
    }
    return true;
  }
  catch(std::exception &e)
  {
    printf("%s\n", e.what());
    printf("%s line %d\n", __FILE__, __LINE__);
    return false;
  }
}

bool FileBackedBigMatrix::connect( const std::string &sharedName, 
  const std::string &fileName, const std::string &filePath, const index_type numRow, 
  const index_type numCol, const int matrixType, 
  const bool sepCols)
{
  try
  {
    _sharedName=sharedName;
    _fileName=fileName;
    _nrow = numRow;
    _totalRows = _nrow;
    _ncol = numCol;
    _totalCols = _ncol;
    _matType = matrixType;
    _sepCols = sepCols;
    _filePath = filePath;
    if (_sepCols)
    {
      switch(_matType)
      {
        case 1:
          _pdata = ConnectFileBackedSepMatrix<char>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
          break;
        case 2:
          _pdata = ConnectFileBackedSepMatrix<short>(_fileName, filePath,
          _dataRegionPtrs, _nrow, _ncol);
          break;
        case 4:
          _pdata = ConnectFileBackedSepMatrix<int>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
          break;
        case 8:
          _pdata = ConnectFileBackedSepMatrix<double>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
      }
    }
    else
    {
      switch(_matType)
      {
        case 1:
          _pdata = ConnectFileBackedMatrix<char>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
          break;
        case 2:
          _pdata = ConnectFileBackedMatrix<short>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
          break;
        case 4:
          _pdata = ConnectFileBackedMatrix<int>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
          break;
        case 8:
          _pdata = ConnectFileBackedMatrix<double>(_fileName, filePath,
            _dataRegionPtrs, _nrow, _ncol);
      }
    }
    if (!_pdata)
    {
      return false;
    }
    return true;
  }
  catch(std::exception &e)
  {
    printf("%s\n", e.what());
    printf("%s line %d\n", __FILE__, __LINE__);
    return false;
  }
}

void DestroyFileBackedSepMatrix( const std::string &sharedName, 
  const index_type ncol, const std::string &fileName)
{
  index_type i;
  for (i=0; i < ncol; ++i)
  {
    try
    {
      shared_memory_object::remove((sharedName + "_column_" + ttos(i)).c_str());
    }
    catch(std::exception &e)
    {
      printf("%s\n", e.what());
      printf("%s line %d\n", __FILE__, __LINE__);
    }
  }
}

bool FileBackedBigMatrix::destroy()
{
  try
  {
    _dataRegionPtrs.resize(0);
    if (_sepCols) 
    {
      DestroyFileBackedSepMatrix(_sharedName, _totalCols, _fileName);
      if (_pdata) 
      {
        switch(_matType)
        {
          case 1:
            delete [] reinterpret_cast<char**>(_pdata);
            break;
          case 2:
            delete [] reinterpret_cast<short**>(_pdata);
            break;
          case 4:
            delete [] reinterpret_cast<int**>(_pdata);
            break;
          case 8:
            delete [] reinterpret_cast<double**>(_pdata);
            }
        }
    } 
    else // not _sepCols
    { 
      try
      {            
        shared_memory_object::remove(_sharedName.c_str());
      }
      catch(std::exception &e)
      {
        printf("%s\n", e.what());
        printf("%s line %d\n", __FILE__, __LINE__);
      }
    }

        // In all cases, do the following:
    _ncol=0;
    _nrow=0;
    _totalRows=0;
    _totalCols=0;
    _colOffset=0;
    _rowOffset=0;
    _pdata=0;
    _colNames.clear();
    _rowNames.clear();
    return true;

  } // end of the try
  catch(std::exception &e)
  {
    printf("%s\n", e.what());
    printf("%s line %d\n", __FILE__, __LINE__);
    return false;
  }
}
