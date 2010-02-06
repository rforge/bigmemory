#ifndef BIG_MATRIX_ACCESSOR
#define BIG_MATRIX_ACCESSOR

#include "BigMatrix.h"

// The MatrixAccessor class allows the user to access non-separated
// big matrix data as matrix[i][j].
template<typename T>
class MatrixAccessor
{
  public:
    MatrixAccessor( T* pData, const index_type &nrow)
    {
      _pMat = pData;
      _totalRows = nrow;
      _rowOffset = 0;
      _colOffset = 0;
    }

    MatrixAccessor( BigMatrix &bm )
    {
      _pMat = reinterpret_cast<T*>(bm.matrix());
      _totalRows = bm.total_rows();
      _rowOffset = bm.row_offset();
      _colOffset = bm.col_offset();
    }

    inline T* operator[](const index_type &col) 
    {
      return _pMat + _totalRows * (col + _colOffset) + _rowOffset;
    }

  protected:
    T *_pMat;
    index_type _totalRows;
    index_type _rowOffset;
    index_type _colOffset;
};

template<typename T>
class SepMatrixAccessor
{
  public:
    SepMatrixAccessor( BigMatrix &bm)
    {
      _ppMat = reinterpret_cast<T**>(bm.matrix());
      _rowOffset = bm.row_offset();
      _colOffset = bm.col_offset();
    }

    inline T* operator[](const index_type col) 
    {
      return _ppMat[col + _colOffset] + _rowOffset;
    }
  protected:
    T **_ppMat;
    index_type _rowOffset;
    index_type _colOffset;
};

#endif //BIG_MATRIX_ACCESSOR
