#include "bigmemory/BigMatrix.h"
#include "bigmemory/MatrixAccessor.hpp"
#include "bigmemory/isna.hpp"
#include "bigmemory/util.h"

#include <utility>
#include <vector>
#include <algorithm>
#include <map>
#include <boost/shared_ptr.hpp>
#include <iostream>

#include <math.h>

#include <R.h>
#include <Rdefines.h>

#include <iostream>

SEXP StringVec2RChar( const vector<string> &strVec )
{
  if (strVec.empty())
    return NULL_USER_OBJECT;
  SEXP ret = PROTECT(allocVector(STRSXP, strVec.size()));
  vector<string>::size_type i;
  for (i=0; i < strVec.size(); ++i)
  {
    SET_STRING_ELT(ret, i, mkChar(strVec[i].c_str()));
  }
  UNPROTECT(1);
  return ret;
}

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
    // These should be given in sorted order.
    // The NA should appear at the end.
    // useNA indicates that there is an NA
    // and it should be used.
    IndexMapper( T *pFirst, T *pLast, bool useNA )
    {
      _begin = pFirst;
      _end = pLast;
      Mapper<T>::_size = distance(_begin, _end);
      _useNA = useNA;
    } 

    virtual int to_index( const T value ) const
    {
      if (isna(value))
      {
        return _useNA ? std::distance(_begin, _end)+1 : -1;
      }  
      return distance(_begin, std::lower_bound( _begin, 
        _end - static_cast<std::size_t>(_useNA), value));
    }

  protected:
    T* _begin;
    T* _end;
    bool _useNA;
};

template<typename T>
class BreakMapper : public Mapper<T>
{
  public:
    BreakMapper( double min, double max, double numBreaks, bool useNA ) 
      : _min(min), _useNA(useNA)
    {
      _width = max - _min;
      _breakWidth = _width / (numBreaks-1);
      _totalBreaks= numBreaks-1;
      _naIndex = _totalBreaks+1;
      Mapper<T>::_size = _totalBreaks + static_cast<std::size_t>(_useNA);
    }

    virtual int to_index( const T value ) const
    {
      if (isna(value))
      {
        return _useNA ? _naIndex : -1;
      }
      int bin = static_cast<int>(
        (static_cast<double>(value)-_min) / _breakWidth);
      if (bin < 0 || bin > _totalBreaks)
        return -1;
      return bin;
    }

  protected:
    double _min;
    double _width;
    double _breakWidth;
    double _totalBreaks; // The total number of valid breaks (not including NA).
    bool _useNA;
    index_type _naIndex;
    std::vector<int> _bins;
};

template<typename T>
struct NAMaker;

template<>
struct NAMaker<char>
{char operator()() const {return NA_CHAR;}};

template<>
struct NAMaker<short>
{short operator()() const {return NA_SHORT;}};

template<>
struct NAMaker<int>
{int operator()() const {return NA_INTEGER;}};

template<>
struct NAMaker<double>
{double operator()() const {return NA_REAL;}};

template<typename ValueType, typename InputIter>
std::vector<ValueType> get_unique( const InputIter itStart, 
  const InputIter itEnd, const int includeNA )
{
  InputIter it;
  typedef std::vector<ValueType> Values;
  bool naAdded=false;
  NAMaker<ValueType> make_na;
  Values v;
  bool valueAdded=false;
  if (itStart == itEnd)
    return v;
  int i=0;
  for (it = itStart; it != itEnd; ++it)
  {
    if (isna(*it))
    { 
      if (includeNA > 0 && !naAdded)
      {
        v.push_back(make_na());
        naAdded=true;
      }
    }
    else
    {
      if (!valueAdded)
      {
        v.insert(v.begin(), *it);
        valueAdded=true;
      }
      else
      {
        typename Values::iterator cit = std::lower_bound( v.begin(), 
          v.end()- static_cast<std::size_t>(naAdded), *it );
        // If we can't find it, we need to add it.
        if (*cit != *it) 
        {
          v.insert(cit, *it);
        }
      }
    }
  }
  if (includeNA==2 && !naAdded)
  {
    v.push_back(make_na());
  }
  return v;
}

template<typename RType, typename MatrixAccessorType>
SEXP UniqueLevels( MatrixAccessorType m, SEXP columns, 
  SEXP breakSexp, SEXP useNA )
{
  double *pBreaks = NUMERIC_DATA(breakSexp);
  NewVec<RType> RNew;
  VecPtr<RType> RData;
  NAMaker<typename MatrixAccessorType::value_type> make_na;
  typedef std::vector<typename MatrixAccessorType::value_type> Values;
  index_type i,j;
  MatrixAccessor<double> breaks( pBreaks, 3 );
  SEXP ret = PROTECT(NEW_LIST(GET_LENGTH(columns)));
  int protectCount = 1;
  Values v;
  const index_type minRowIndex = 0;
  const index_type maxRowIndex = 1;
  const index_type breaksIndex = 2;

  index_type column;
  for (i=0; i < GET_LENGTH(columns); ++i)
  {
    SEXP sv;
    column = static_cast<index_type>(NUMERIC_DATA(columns)[i])-1;
    if ( !isna(breaks[i][0]) )
    {
      v.resize(breaks[i][breaksIndex]);
      for (j=0; j < breaks[i][breaksIndex]; ++j)
      {
        v[j] = static_cast<typename MatrixAccessorType::value_type>(j);
      }
      if (INTEGER_VALUE(useNA) == 1)
      {
        for (j=0; j < m.nrow(); ++j)
        {
          if (isna(m[column][j]))
          {
            v.push_back(make_na());
            break;
          }
        }
      }
      else if (INTEGER_VALUE(useNA) == 2)
      { 
        v.push_back(make_na());
      }
    }
    else
    {
      v = get_unique<typename MatrixAccessorType::value_type>( 
        (m[column]), (m[column] + m.nrow()), INTEGER_VALUE(useNA) );
    }
    sv = PROTECT(RNew(v.size()));
    ++protectCount;
    std::copy( v.begin(), v.end(), RData(sv) );
    SET_VECTOR_ELT( ret, i, sv );
  }
  UNPROTECT(protectCount);
  return(ret);
}

// For now, assume an index mapper.
template<typename RType, typename MatrixAccessorType>
SEXP TAPPLY( MatrixAccessorType m, SEXP columns, SEXP breakSexp,
  SEXP returnTable, SEXP useNA, 
  SEXP returnSummary, SEXP processColumns, SEXP summaryNARM,
  SEXP splitcol, SEXP splitlist )
{
  std::vector<std::string> retNames;
  retNames.push_back(std::string("levels"));
  SEXP uniqueLevels=PROTECT(UniqueLevels<RType>(m, columns, breakSexp, useNA));
  std::map<std::string, int> lmi;
  int i, j, k;
  i=0;
  lmi["levels"] = i++;
  if ( splitcol != NULL_USER_OBJECT )
  {
    lmi["split"] = i++;
    retNames.push_back(std::string("split"));
  }
  if ( LOGICAL_VALUE(returnTable) )
  {
    lmi["table"] = i++;
    retNames.push_back(std::string("table"));
  }
  if ( LOGICAL_VALUE(returnSummary) )
  {
    lmi["summary"] = i++;
    retNames.push_back(std::string("summary"));
  }
  
  SEXP ret = PROTECT(NEW_LIST(i));
  setAttrib( ret, R_NamesSymbol, StringVec2RChar( retNames ) );
  SET_VECTOR_ELT( ret, lmi[string("levels")], uniqueLevels );
  MatrixAccessor<double> breaks( NUMERIC_DATA(breakSexp), 3 );
  typedef boost::shared_ptr<Mapper<RType> > MapperPtr;
  typedef std::vector<MapperPtr> Mappers;
  VecPtr<RType> RData;
  NewVec<RType> RNew;
  Mappers mappers;
  int protectCount=2;
  int totalListSize =0;
  std::vector<int> accMult;
  // Create the data structures that map values to indices for each of the
  // columns.
  for (i=0; i < GET_LENGTH(uniqueLevels); ++i)
  {
    SEXP vec = VECTOR_ELT(uniqueLevels, i);
    int vecLen = GET_LENGTH(vec);
    if (!isna(breaks[i][0]))
    {
      mappers.push_back( MapperPtr(
        new BreakMapper<RType>( breaks[i][0], breaks[i][1], 
          breaks[i][2], INTEGER_VALUE(useNA) > 0 ) ) );
    }
    else
    {
      mappers.push_back( MapperPtr( 
        new IndexMapper<RType>( RData(vec), 
          RData(vec) + vecLen, INTEGER_VALUE(useNA) > 0 ) ) );
    }
    totalListSize = (totalListSize == 0 ? vecLen : totalListSize*vecLen);
    if (i==0)
      accMult.push_back( vecLen );
    else
    {
      accMult.push_back( mappers[i]->size() * accMult[i-1]);
    }
  }
  typedef std::vector<double> Indices;
  typedef std::vector<Indices> TableIndices;
  TableIndices tis;

  typedef std::vector<RType> Values;
  typedef std::vector<Values> TableIndexValues;
  TableIndexValues tiv;

  Indices tvs(totalListSize, 0);
  
  typedef std::vector<double> TableSummary;
  typedef std::vector<TableSummary> TableSummaries;
  std::vector<TableSummaries> ts;
  
  typedef std::vector<index_type> ProcessColumns;
  ProcessColumns procCols;
 
  if ( splitcol != NULL_USER_OBJECT )
  {
    if ( isna(NUMERIC_VALUE(splitcol)) )
    {
      tis.resize(totalListSize);
      std::cout << "tis resized to " << tis.size() << std::endl;
    }
    else
      tiv.resize(totalListSize);
  }
  if ( LOGICAL_VALUE(returnTable) || LOGICAL_VALUE(returnSummary) )
  {
    tvs.resize(totalListSize);
    std::fill( tvs.begin(), tvs.end(), 0 );
  }
  if ( LOGICAL_VALUE(returnSummary) )
  {
    procCols.resize(GET_LENGTH(processColumns));
    for (k=0; k < procCols.size(); ++k)
    {
      procCols[k] = static_cast<index_type>(NUMERIC_DATA(processColumns)[k])-1;
    }
    ts.resize( procCols.size() );
    // min, max, sum, sum^2
    std::fill( ts.begin(), ts.end(), 
      TableSummaries(totalListSize, TableSummary(7, 0)) );
  }
  // Get the indices for each of the column-value combinations.
  for (i=0; i < m.nrow(); ++i)
  {
    int tableIndex=0;
    int mapperVal;
    for (j=1; j < GET_LENGTH(columns); ++j)
    {
      mapperVal = mappers[j]->to_index( static_cast<RType>(
          (m[static_cast<index_type>(NUMERIC_DATA(columns)[j]-1)][i])) );
      if (mapperVal == -1)
      {
        tableIndex = -1;
        break;
      }
      tableIndex += accMult[j-1] * mapperVal;
    }
    mapperVal = mappers[0]->to_index( static_cast<RType>(
        (m[static_cast<index_type>(NUMERIC_DATA(columns)[0]-1)][i])) );
    if (tableIndex == -1 || mapperVal == -1)
      continue;
    tableIndex += mapperVal;
    if ( splitcol != NULL_USER_OBJECT )
    {
      if ( isna(NUMERIC_VALUE(splitcol)) )
        tis[tableIndex].push_back(i+1);
      else
        tiv[tableIndex].push_back( 
          m[static_cast<index_type>(NUMERIC_VALUE(splitcol))][i] );
    }
    if ( LOGICAL_VALUE(returnTable) || LOGICAL_VALUE(returnSummary) )
    {
      ++tvs[tableIndex];
    }
    if ( LOGICAL_VALUE(returnSummary) )
    {
      for (k=0; k < ts.size(); ++k)
      {
        TableSummaries &ss = ts[k];
        double matVal = static_cast<double>(
          m[static_cast<index_type>(procCols[k])][i]);
        if (isna(static_cast<typename MatrixAccessorType::value_type>(matVal)))
        {
          ++ss[tableIndex][6];
          if (!LOGICAL_VALUE(summaryNARM))
            continue;
        }
        if (ss[tableIndex][4] == 0) 
        {
          ss[tableIndex][0] = matVal;
          ss[tableIndex][4] = 1;
        }
        else
        {
          if (ss[tableIndex][0] > matVal)
            ss[tableIndex][0] = matVal;
        }
        if (ss[tableIndex][5] == 0) 
        {
          ss[tableIndex][1] = matVal;
          ss[tableIndex][5] = 1;
        }
        else
        {
          if (ss[tableIndex][1] < matVal)
            ss[tableIndex][1] = matVal;
        }
        ss[tableIndex][2] += matVal;
        ss[tableIndex][3] += pow(matVal, 2.0);
      }
    }
  }
  if ( splitcol != NULL_USER_OBJECT )
  { 
    SEXP mapRet;
    if ( LOGICAL_VALUE(splitlist) )
    {
      SEXP vec;
      // Copy to a list of vectors that R can read.
      if ( isna(NUMERIC_VALUE(splitcol)) )
      {
        mapRet = PROTECT(NEW_LIST( tis.size() ));
        ++protectCount;
        for (i=0; i < tis.size(); ++i)
        {
          Indices &ind = tis[i];
          vec = PROTECT(NEW_NUMERIC(tis[i].size()));
          ++protectCount;
          std::copy( ind.begin(), ind.end(), RData(vec) );
          SET_VECTOR_ELT( mapRet, i, vec );
        }
      }
      else
      {
        mapRet = PROTECT(NEW_LIST( tiv.size() ));
        ++protectCount;
        index_type outCol = static_cast<index_type>(NUMERIC_VALUE(splitcol));
        for (i=0; i < tiv.size(); ++i)
        {
          Values &ind = tiv[i];
          vec = PROTECT(RNew(tiv[i].size()));
          ++protectCount;
          std::copy( ind.begin(), ind.end(), RData(vec) );
          SET_VECTOR_ELT( mapRet, i, vec );
        }
      }
    }
    else
    {
      mapRet = NEW_NUMERIC(m.nrow());
      double *pmr = NUMERIC_DATA(mapRet);
      for (i=0; i < tis.size(); ++i)
      {
        Indices &inds = tis[i];
        for (j=0; j < inds.size(); ++j)
        {
          pmr[j] = i;
        }
      }
    }
    SET_VECTOR_ELT(ret, lmi[string("split")], mapRet);
  }
  if ( LOGICAL_VALUE(returnTable) )
  {
    SEXP tableRet = PROTECT(NEW_INTEGER(tvs.size()));
    ++protectCount;
    std::copy( tvs.begin(), tvs.end(), INTEGER_DATA(tableRet) );
    SET_VECTOR_ELT(ret, lmi[string("table")], tableRet);
  } 
  if ( LOGICAL_VALUE(returnSummary) )
  {
    // If we change the data structures holding the summaries, can
    // we get better performance.
    std::vector<std::string> colnames;
    colnames.push_back(string("min"));
    colnames.push_back(string("max"));
    colnames.push_back(string("mean"));
    colnames.push_back(string("sd"));
    colnames.push_back(string("NAs"));
    SEXP summaryRet = PROTECT(NEW_LIST(ts[0].size()));
    ++protectCount;
    for (i=0; i < GET_LENGTH(summaryRet); ++i)
    {
      SEXP dimnames = PROTECT(NEW_LIST(2));
      ++protectCount;
      SET_VECTOR_ELT(dimnames, 0, R_NilValue );
      SET_VECTOR_ELT(dimnames, 1, StringVec2RChar(colnames) );
      
      SEXP retMat = allocMatrix(REALSXP, ts.size(), 5);
      setAttrib(retMat, R_DimNamesSymbol, dimnames);
      MatrixAccessor<double> rm( NUMERIC_DATA(retMat), ts.size() );
      for (j=0; j < ts.size(); ++j)
      {
        if (tvs[i] > 0)
        {
          rm[0][j] = ts[j][i][0];
          rm[1][j] = ts[j][i][1];
          rm[2][j] = ts[j][i][2] / static_cast<double>(tvs[i]);
          rm[3][j] = ts[j][i][3] / static_cast<double>(tvs[i]) -
            pow( rm[2][j], 2.0 );
          rm[4][j] = ts[j][i][6];
        }
        else
        {
          rm[0][j] = NA_REAL;
          rm[1][j] = NA_REAL;
          rm[2][j] = NA_REAL;
          rm[3][j] = NA_REAL;
          rm[4][j] = 0;
        }
      }
      SET_VECTOR_ELT(summaryRet, i, retMat);
    }
    SET_VECTOR_ELT(ret, lmi[string("summary")], summaryRet);
  }
  UNPROTECT( protectCount );
  return ret;
}

extern "C"
{

SEXP RNumericTAPPLY( SEXP numericMatrix , SEXP columns, SEXP breaks,
  SEXP returnTable, SEXP useNA, SEXP returnSummary, 
  SEXP processColumns, SEXP summaryNARM, SEXP splitcol, SEXP splitlist )
{
  return TAPPLY<double>( MatrixAccessor<double>( NUMERIC_DATA(numericMatrix),
    static_cast<index_type>(Rf_nrows(numericMatrix)) ), 
    columns, breaks, returnTable, useNA, 
    returnSummary, processColumns, summaryNARM, splitcol, splitlist );
}

SEXP RIntTAPPLY( SEXP numericMatrix , SEXP columns, SEXP breaks,
  SEXP returnTable, SEXP useNA, SEXP returnSummary, 
  SEXP processColumns, SEXP summaryNARM, SEXP splitcol, SEXP splitlist )
{
  return TAPPLY<int>( MatrixAccessor<int>( INTEGER_DATA(numericMatrix),
    static_cast<index_type>(Rf_nrows(numericMatrix)) ), 
    columns, breaks, returnTable, useNA, 
    returnSummary, processColumns, summaryNARM, splitcol, splitlist );
}

// Return both the table indices in a list and the unique levels
//useNa =0 "no", 1 "ifany", 2 "always".
//ccols breaks, boolean return.map, boolean table, integer useNA,
//boolean summary?, numeric summary columns, boolean summary.na.rm
SEXP BigMatrixTAPPLY( SEXP bigMatAddr, SEXP columns, SEXP breaks, 
  SEXP returnTable, SEXP useNA, SEXP returnSummary, 
  SEXP processColumns, SEXP summaryNARM, SEXP splitcol, SEXP splitlist )
{
  BigMatrix *pMat = reinterpret_cast<BigMatrix*>(
    R_ExternalPtrAddr(bigMatAddr));
  if (pMat->separated_columns())
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return TAPPLY<int>( SepMatrixAccessor<char>(*pMat), columns, 
          breaks, processColumns, returnTable, useNA,
          returnSummary, summaryNARM, splitcol, splitlist );
      case 2:
        return TAPPLY<int>( SepMatrixAccessor<short>(*pMat), columns,
          breaks, processColumns, returnTable, useNA,
          returnSummary, summaryNARM, splitcol, splitlist );
      case 4:
        return TAPPLY<int>( SepMatrixAccessor<int>(*pMat), columns,
          breaks, processColumns, returnTable, useNA,
          returnSummary, summaryNARM, splitcol, splitlist );
      case 8:
        return TAPPLY<double>( SepMatrixAccessor<double>(*pMat), columns,
          breaks, processColumns, returnTable, useNA,
          returnSummary, summaryNARM, splitcol, splitlist );
    }
  }
  else
  {
    switch (pMat->matrix_type())
    {
      case 1:
        return TAPPLY<int>( MatrixAccessor<char>(*pMat), columns,
          breaks, processColumns, returnTable, useNA,
          returnSummary, summaryNARM, splitcol, splitlist );
      case 2:
        return TAPPLY<int>( MatrixAccessor<short>(*pMat), columns, 
          breaks, processColumns, returnTable, useNA,
          returnSummary, summaryNARM, splitcol, splitlist );
      case 4:
        return TAPPLY<int>( MatrixAccessor<int>(*pMat), columns, 
          breaks, processColumns, returnTable, useNA,
          returnSummary, summaryNARM, splitcol, splitlist );
      case 8:
        return TAPPLY<double>( MatrixAccessor<double>(*pMat), columns, 
          breaks, processColumns, returnTable, useNA,
          returnSummary, summaryNARM, splitcol, splitlist );
    }
  }
  return R_NilValue;
}

}
