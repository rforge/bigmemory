#include "bigmemory/BigMatrix.h"
#include "bigmemory/MatrixAccessor.hpp"
#include "bigmemory/util.h"

#include <utility>
#include <vector>
#include <algorithm>
#include <boost/shared_ptr.hpp>
#include <iostream>

#include <R.h>
#include <Rdefines.h>

template<typename T>
class Mapper
{
  public:
    typedef std::size_t size_type;

  public:
    virtual int to_index( const T value ) const=0;

    size_type size() const {return _size;}

  protected:
    size_type _size;
};

template<typename T>
class IndexMapper : public Mapper<T>
{
  public:
    typedef vector<int> Levels;

  public:
    IndexMapper( T *pFirst, T *pLast )
    {
      _begin = pFirst;
      _end = pLast;
      std::sort( _begin, _end );
      Mapper<T>::_size = distance(_begin, _end);
    } 

    virtual int to_index( const T value ) const
    {
      return distance(std::lower_bound( _begin, _end, value ), _begin);
    }

  protected:
    T* _begin;
    T* _end;
};


template<typename ValueType, typename InputIter>
std::vector<ValueType> get_unique( const InputIter itStart, 
  const InputIter itEnd )
{
  InputIter it;
  typedef std::vector<ValueType> Values;
  Values v;
  if (itStart == itEnd)
    return v;
  v.push_back( *itStart );
  
  for (it = itStart+1; it != itEnd; ++it)
  {
    typename Values::iterator cit = std::lower_bound( v.begin(), v.end(), *it );
    // If we can't find it, we need to add it.
    if (*cit != *it) 
    {
      v.insert(cit, *it);
    }
  }
}

template<typename RType, typename MatrixAccessor>
SEXP UniqueLevels( MatrixAccessor m, SEXP columns )
{
  NewVec<RType> RNew;
  VecPtr<RType> RData;
  typedef std::vector<typename MatrixAccessor::value_type> Values;
  index_type i;
  SEXP ret = PROTECT(NEW_LIST(GET_LENGTH(columns)));
  int protectCount = 1;
  Values v;
  for (i=0; i < GET_LENGTH(columns); ++i)
  {
    index_type column = static_cast<index_type>(NUMERIC_DATA(columns)[i])-1;
    v = get_unique<typename MatrixAccessor::value_type>( 
      (m[column]), (m[column] + m.nrow()) );
    SEXP sv = PROTECT(RNew(v.size()));
    ++protectCount;
    std::copy( v.begin(), v.end(), RData(sv) );
    SET_VECTOR_ELT( ret, i, sv );
  }
  UNPROTECT(protectCount);
  return(ret);
}

// For now, assume an index mapper.
template<typename RType, typename MatrixAccessor>
SEXP TableIndices( MatrixAccessor m, SEXP uniqueLevels, SEXP columns )
{
  typedef boost::shared_ptr<Mapper<RType> > 
    MapperPtr;
  typedef std::vector<MapperPtr> Mappers;
  VecPtr<RType> RData;
  NewVec<RType> RNew;
  Mappers mappers;
  int i;
  int totalListSize =0;
  std::vector<int> accMult;
  // Create the data structures that map values to indices for each of the
  // columns.
  for (i=0; i < GET_LENGTH(uniqueLevels); ++i)
  {
    SEXP vec = VECTOR_ELT(uniqueLevels, i);
    int vecLen = GET_LENGTH(vec);
    mappers.push_back( MapperPtr( 
      new IndexMapper<RType>( RData(vec), 
        RData(vec) + vecLen ) ) );
    totalListSize = (totalListSize == 0 ? vecLen : totalListSize*vecLen);
    if (i==0)
      accMult.push_back( vecLen );
    else
    {
      accMult.push_back( mappers[i]->size() * accMult[i-1]);
    }
  }

  typedef std::vector<int> Indices;
  typedef std::vector<Indices> TableIndices;
  TableIndices ti( totalListSize );
  int j; 
  // Get the indices for each of the column-value combinations.
  for (i=0; i < m.nrow(); ++i)
  {
    int tableIndex=0;
    for (j=1; j < GET_LENGTH(columns); ++j)
    {
      tableIndex += accMult[j-1] * mappers[j]->to_index(
        static_cast<RType>(
          (m[static_cast<index_type>(NUMERIC_DATA(columns)[j]-1)][i])) );
    }
    tableIndex += mappers[0]->to_index(
      static_cast<RType>(
        (m[static_cast<index_type>(NUMERIC_DATA(columns)[0]-1)][i])) );
    ti[tableIndex].push_back(i+1);
  }
  
  SEXP ret = PROTECT(NEW_LIST( ti.size() ));
  SEXP vec;
  // Copy to a list of vectors that R can read.
  for (i=0; i < ti.size(); ++i)
  {
    Indices &ind = ti[i];
    vec = PROTECT(RNew(ti[i].size()));
    std::copy( ind.begin(), ind.end(), RData(vec) );
    SET_VECTOR_ELT( ret, i, vec );
  }
  UNPROTECT( 1+ti.size() );
  return ret;
}

extern "C"
{

SEXP UniqueLevels( SEXP bigMatAddr, SEXP columns )
{
  BigMatrix *pMat =
    reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return UniqueLevels<int>(
          SepMatrixAccessor<char>(*pMat), columns );
      case 2:
        return UniqueLevels<int>(
          SepMatrixAccessor<short>(*pMat), columns );
      case 4:
        return UniqueLevels<int>(
          SepMatrixAccessor<int>(*pMat), columns );
      case 8:
        return UniqueLevels<double>(
          SepMatrixAccessor<double>(*pMat), columns );
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return UniqueLevels<int>(
          MatrixAccessor<char>(*pMat), columns );
      case 2:
        return UniqueLevels<int>(
          MatrixAccessor<short>(*pMat), columns );
      case 4:
        return UniqueLevels<int>(
          MatrixAccessor<int>(*pMat), columns );
      case 8:
        return UniqueLevels<double>(
          MatrixAccessor<double>(*pMat), columns );
    }
  }
  return R_NilValue;
}

SEXP TableIndices( SEXP bigMatAddr, SEXP columns )
{
  SEXP uniqueLevels = UniqueLevels(bigMatAddr, columns);
  BigMatrix *pMat =
    reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return TableIndices<int>( SepMatrixAccessor<char>(*pMat), 
          uniqueLevels, columns );
      case 2:
        return TableIndices<int>( SepMatrixAccessor<short>(*pMat), 
          uniqueLevels, columns );
      case 4:
        return TableIndices<int>( SepMatrixAccessor<int>(*pMat), 
          uniqueLevels, columns );
      case 8:
        return TableIndices<double>( SepMatrixAccessor<double>(*pMat), 
          uniqueLevels, columns );
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return TableIndices<int>( MatrixAccessor<char>(*pMat), 
          uniqueLevels, columns );
      case 2:
        return TableIndices<int>( MatrixAccessor<short>(*pMat), 
          uniqueLevels, columns );
      case 4:
        return TableIndices<int>( MatrixAccessor<int>(*pMat), 
          uniqueLevels, columns );
      case 8:
        return TableIndices<double>( MatrixAccessor<double>(*pMat), 
          uniqueLevels, columns );
    }
  }
  return R_NilValue;
}

}
