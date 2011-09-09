#ifndef BIGALGEBRA_PTR_UTIL
#define BIGALGEBRA_PTR_UTIL
#include "bigalgebra/bigalgebra.h"

double* make_double_ptr( SEXP matrix, SEXP isBigMatrix  )
{
  double *matrix_ptr;
    
  if (LOGICAL_VALUE(isBigMatrix) == (Rboolean)TRUE) // Big Matrix
  {
    SEXP address = GET_SLOT(matrix, install("address"));
    BigMatrix *pbm = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(address));
    if (!pbm)
      return(NULL);
    
    // Check that have acceptable big.matrix
    if (pbm->row_offset() > 0 && pbm->ncol() > 1)
    {
      std::string errMsg = string("sub.big.matrix objects cannoth have row ") +
        string("offset greater than zero and number of columns greater than 1");
      Rf_error(errMsg.c_str());
      return(NULL);
    }
    
    index_type offset = pbm->nrow() * pbm->col_offset();
    matrix_ptr = reinterpret_cast<double*>(pbm->matrix()) + offset;
  }
  else  // Regular R Matrix
  {
    matrix_ptr = NUMERIC_DATA(matrix);
  }
  
  return(matrix_ptr);
};

#endif //BIGALGEBRA_PTR_UTIL
