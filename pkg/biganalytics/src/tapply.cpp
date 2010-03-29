#include "bigmemory/BigMatrix.h"
#include "bigmemory/MatrixAccessor.hpp"
#include "bigmemory/util.h"

#include <utility>
#include <vector>
#include <algorithm>
#include <map>
#include <boost/shared_ptr.hpp>

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
    IndexMapper( T *pFirst, T *pLast )
    {
      _begin = pFirst;
      _end = pLast;
      std::sort( _begin, _end );
      Mapper<T>::_size = distance(_begin, _end);
    } 

    virtual int to_index( const T value ) const
    {
      return distance(_begin, std::lower_bound( _begin, _end, value ));
    }

  protected:
    T* _begin;
    T* _end;
};

template<typename T>
class BreakMapper : public Mapper<T>
{
  public:
    BreakMapper( double min, double max, double numBreaks ) : _min(min) 
    {
      _width = max - _min;
      _breakWidth = _width / (numBreaks+1);
      _numBreaks = numBreaks;
    }

    virtual int to_index( const T value ) const
    {
      int bin = (value-min) / _breakWidth;
      if (bin < 0 || bin > _numBreaks+1)
        return NA_INTEGER;
      return bin;
    }

  protected:
    double _min;
    double _width;
    double _breakWidth;
    double _numBreaks;
    std::vector<int> _bins;
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
SEXP TAPPLY( MatrixAccessor m, SEXP uniqueLevels, SEXP columns, 
  SEXP processColumns, bool makeMap, bool makeTable, bool makeSummary,
  std::map<std::string, int> &lmi, SEXP ret )
{
  typedef boost::shared_ptr<Mapper<RType> > 
    MapperPtr;
  typedef std::vector<MapperPtr> Mappers;
  VecPtr<RType> RData;
  NewVec<RType> RNew;
  Mappers mappers;
  int i, j, k;
  int protectCount=2;
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
  TableIndices tis;
  Indices tvs(totalListSize, 0);
  
  typedef std::vector<LDOUBLE> TableSummary;
  typedef std::vector<TableSummary> TableSummaries;
  std::vector<TableSummaries> ts;
  
  typedef std::vector<index_type> ProcessColumns;
  ProcessColumns procCols;
  
  if (makeMap)
  {
    tis.resize(totalListSize);
  }
  if (makeTable || makeSummary)
  {
    tvs.resize(totalListSize);
    std::fill( tvs.begin(), tvs.end(), 0 );
  }
  if (makeSummary)
  {
    procCols.resize(GET_LENGTH(processColumns));
    for (k=0; k < procCols.size(); ++k)
    {
      procCols[i] = static_cast<index_type>(NUMERIC_DATA(processColumns)[k])-1;
    }
    ts.resize( procCols.size() );
    // min, max, sum, sum^2
    std::fill( ts.begin(), ts.end(), 
      TableSummaries(totalListSize, TableSummary(4, 0)) );
  }
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
    if (makeMap)
    {
      tis[tableIndex].push_back(i+1);
    }
    if (makeTable || makeSummary)
    {
      ++tvs[tableIndex];
    }
    if (makeSummary)
    {
      for (k=0; k < ts.size(); ++k)
      {
        TableSummaries &ss = ts[i];
        double matVal = static_cast<double>(m[i][procCols[k]]);
        if (k==0)
        {
          ss[tableIndex][0] = matVal;
          ss[tableIndex][1] = matVal;
        }
        else
        {
          if (ss[tableIndex][0] > matVal) ss[tableIndex][0] = matVal;
          if (ss[tableIndex][1] < matVal) ss[tableIndex][1] = matVal;
        }
        ss[tableIndex][2] += matVal;
        ss[tableIndex][3] += matVal*matVal;
      }
    }
  }
 
  if (makeMap)
  { 
    SEXP mapRet = PROTECT(NEW_LIST( tis.size() ));
    ++protectCount;
    SEXP vec;
    // Copy to a list of vectors that R can read.
    for (i=0; i < tis.size(); ++i)
    {
      Indices &ind = tis[i];
      vec = PROTECT(RNew(tis[i].size()));
      ++protectCount;
      std::copy( ind.begin(), ind.end(), RData(vec) );
      SET_VECTOR_ELT( ret, i, vec );
    }
    SET_VECTOR_ELT(ret, lmi[string("map")], mapRet);
  }
  if (makeTable)
  {
    SEXP tableRet = PROTECT(NEW_INTEGER(tvs.size()));
    ++protectCount;
    std::copy( tvs.begin(), tvs.end(), INTEGER_DATA(tableRet) );
    SET_VECTOR_ELT(ret, lmi[string("table")], tableRet);
  }   
  if (makeSummary)
  {
    //TODO: Start here.
  }
  UNPROTECT( protectCount );
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

// Return both the table indices in a list and the unique levels.
SEXP BigMatrixTAPPLY( SEXP bigMatAddr, SEXP columns, SEXP processColumns,
  SEXP returnMap, SEXP returnTable, SEXP returnSummary )
{
  SEXP uniqueLevels = PROTECT(UniqueLevels(bigMatAddr, columns));
  BigMatrix *pMat =
    reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(bigMatAddr));
  int retSize = static_cast<int>(LOGICAL_VALUE(returnMap)) + 
    static_cast<int>(LOGICAL_VALUE(returnTable)) +
    static_cast<int>(LOGICAL_VALUE(returnSummary));
  std::map<std::string, int> lmi;
  int i=0;
  lmi["levels"] = i++;
  if ( LOGICAL_VALUE(returnMap) )
    lmi["map"] = i++;
  if ( LOGICAL_VALUE(returnTable) )
    lmi["table"] = i++;
  if ( LOGICAL_VALUE(returnSummary) )
    lmi["summary"] = i++;
  
  SEXP ret = PROTECT(NEW_LIST(retSize));
  // TODO: Add names to the return list;
  SET_VECTOR_ELT( ret, 0, uniqueLevels );
  SEXP second;
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return TAPPLY<int>( SepMatrixAccessor<char>(*pMat), 
          uniqueLevels, columns, processColumns,
          static_cast<bool>(LOGICAL_VALUE(returnMap)),
          static_cast<bool>(LOGICAL_VALUE(returnTable)),
          static_cast<bool>(LOGICAL_VALUE(returnSummary)),
          lmi, ret );
        break;
      case 2:
        return TAPPLY<int>( SepMatrixAccessor<short>(*pMat), 
          uniqueLevels, columns, processColumns,
          static_cast<bool>(LOGICAL_VALUE(returnMap)),
          static_cast<bool>(LOGICAL_VALUE(returnTable)),
          static_cast<bool>(LOGICAL_VALUE(returnSummary)),
          lmi, ret );
        break;
      case 4:
        return TAPPLY<int>( SepMatrixAccessor<int>(*pMat), 
          uniqueLevels, columns, processColumns,
          static_cast<bool>(LOGICAL_VALUE(returnMap)),
          static_cast<bool>(LOGICAL_VALUE(returnTable)),
          static_cast<bool>(LOGICAL_VALUE(returnSummary)),
          lmi, ret );
      case 8:
        return TAPPLY<double>( SepMatrixAccessor<double>(*pMat),
          uniqueLevels, columns, processColumns,
          static_cast<bool>(LOGICAL_VALUE(returnMap)),
          static_cast<bool>(LOGICAL_VALUE(returnTable)),
          static_cast<bool>(LOGICAL_VALUE(returnSummary)),
          lmi, ret );
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return TAPPLY<int>( MatrixAccessor<char>(*pMat), 
          uniqueLevels, columns, processColumns,
          static_cast<bool>(LOGICAL_VALUE(returnMap)),
          static_cast<bool>(LOGICAL_VALUE(returnTable)),
          static_cast<bool>(LOGICAL_VALUE(returnSummary)),
          lmi, ret );
      case 2:
        return TAPPLY<int>( MatrixAccessor<short>(*pMat), 
          uniqueLevels, columns, processColumns,
          static_cast<bool>(LOGICAL_VALUE(returnMap)),
          static_cast<bool>(LOGICAL_VALUE(returnTable)),
          static_cast<bool>(LOGICAL_VALUE(returnSummary)),
          lmi, ret );
      case 4:
        return TAPPLY<int>( MatrixAccessor<int>(*pMat), 
          uniqueLevels, columns, processColumns,
          static_cast<bool>(LOGICAL_VALUE(returnMap)),
          static_cast<bool>(LOGICAL_VALUE(returnTable)),
          static_cast<bool>(LOGICAL_VALUE(returnSummary)),
          lmi, ret );
      case 8:
        return TAPPLY<double>( MatrixAccessor<double>(*pMat), 
          uniqueLevels, columns, processColumns,
          static_cast<bool>(LOGICAL_VALUE(returnMap)),
          static_cast<bool>(LOGICAL_VALUE(returnTable)),
          static_cast<bool>(LOGICAL_VALUE(returnSummary)),
          lmi, ret );
    }
  }
  return ret;
}

}
