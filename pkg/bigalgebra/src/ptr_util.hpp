#ifndef BIGALGEBRA_PTR_UTIL
#define BIGALGEBRA_PTR_UTIL
#include <string>
#include "BigMatrix.h"

#include <R.h>
#include <Rinternals.h>
#include <Rdefines.h>

double* make_double_ptr( SEXP matrix, SEXP isBigMatrix  )
{
  if (LOGICAL_VALUE(isBigMatrix) == (Rboolean)TRUE)
  {
    SEXP address = GET_SLOT(matrix, install("address"));
    BigMatrix *pbm = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(address));
    if (!pbm)
    {
      return(NULL);
    }
//  Functionality to support big.sub.matrices
//    if (pbm->row_offset() > 0 && pbm->num_columns() > 1)
//    {
//      std::string errMsg = string("big.sub.matrix objects cannoth have row ") +
//        string("offset greater than zero and number of columns greater than 1");
//      Rf_error(errMsg.c_str());
//      return(NULL);
//    }
    return(reinterpret_cast<double*>(pbm->matrix()) 
      + pbm->col_offset()*pbm->nrow() + pbm->row_offset());
  }
  return(NUMERIC_DATA(matrix));
};

int* make_int_ptr( SEXP matrix, SEXP isBigMatrix )
{
  if (LOGICAL_VALUE(isBigMatrix) == (Rboolean)TRUE)
  {
    SEXP address = GET_SLOT(matrix, install("address"));
    BigMatrix *pbm = reinterpret_cast<BigMatrix*>(R_ExternalPtrAddr(address));
    if (!pbm)
    {
      return(NULL);
    }
// Functionality to support big.sub.matrices
//    if (pbm->row_offset() > 0 && pbm->num_columns() > 1)
//    {
//      std::string errMsg = string("big.sub.matrix objects cannoth have row ") +
//        string("offset greater than zero and number of columns greater than 1");
//      Rf_error(errMsg.c_str());
//      return(NULL);
//    }
    return(reinterpret_cast<int*>(pbm->matrix()) 
      + pbm->col_offset()*pbm->nrow() + pbm->row_offset());
  }
  return(INTEGER_DATA(matrix));
};

#endif //BIGALGEBRA_PTR_UTIL
