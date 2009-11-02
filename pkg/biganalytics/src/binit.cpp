#include <iostream> // hack to make sure we are using the right "length"
                    // function
#include "bigmemory/BigMatrix.h"
#include "bigmemory/MatrixAccessor.hpp"
#include "bigmemory/bigmemoryDefines.h"
#include "bigmemory/isna.hpp"

#include <math.h>
#include <R.h>
#include <Rdefines.h>

template<typename dataT>
void CBinIt2work(dataT *pc1, dataT *pc2, index_type nr, double *pRet,
                 double *pB1, double *pB2)
{
  index_type i, j, k;
  double nbins1 = pB1[2];
  double nbins2 = pB2[2];
  double min1 = pB1[0];
  double min2 = pB2[0];
  double max1 = pB1[1];
  double max2 = pB2[1];
  int good;

  for (i=0; i<(index_type)nbins1; i++) {
    for (j=0; j<(index_type)nbins2; j++) {
      pRet[j*((index_type)nbins1)+i] = 0.0;
    }
  }
  
  for (k=0; k<nr; k++) {
    if ( !isna(pc1[k]) && !isna(pc2[k]) ){
      good = 1;
      if ( (((double)pc1[k])>=min1) && (((double)pc1[k])<=max1) ) {
        i = (index_type) ( nbins1 * (((double)pc1[k])-min1) / (max1-min1) );
        if (i==(index_type)nbins1) i--;
      } else { good = 0; }
      if ( (((double)pc2[k])>=min2) & (((double)pc2[k])<=max2) ) {
        j = (index_type) ( nbins2 * (((double)pc2[k])-min2) / (max2-min2) );
        if (j==(index_type)nbins2) j--;
      } else { good = 0; }
      if (good == 1) {
        pRet[j*((index_type)nbins1)+i]++;
      }
    } // End only do work in there isn't an NA value
  } // End looping over all rows.

}

template<typename dataT>
void CBinIt1work(dataT *pc, index_type nr, double *pRet, double *pB)
{
  index_type i, k;
  double nbins = pB[2];
  double min = pB[0];
  double max = pB[1];
  int good;

  for (i=0; i<(index_type)nbins; i++) {
    pRet[i] = 0.0;
  }
 
  for (k=0; k<nr; k++) {
    if ( !isna(pc[k]) ){
      good = 1;
      if ( (((double)pc[k])>=min) && (((double)pc[k])<=max) ) {
        i = (index_type) ( nbins * (((double)pc[k])-min) / (max-min) );
        if (i==(index_type)nbins) i--;
      } else { good = 0; }
      if (good == 1) {
        pRet[i]++;
      }
    } // End only do work in there isn't an NA value
  } // End looping over all rows.

}


template<typename dataT>
void CBinIt2(SEXP bigMatrixAddr, double *pRet, double *pCols,
             double *pB1, double *pB2)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatrixAddr);
  if (pMat->separated_columns()) {
    SepMatrixAccessor<dataT> Mat(*pMat);
    CBinIt2work(Mat[(index_type)pCols[0]-1], Mat[(index_type)pCols[1]-1], pMat->nrow(),
                pRet, pB1, pB2);
  } else {
    MatrixAccessor<dataT> Mat(*pMat);
    CBinIt2work(Mat[(index_type)pCols[0]-1], Mat[(index_type)pCols[1]-1], pMat->nrow(),
                pRet, pB1, pB2);
  }
}

template<typename dataT>
void CBinIt1(SEXP bigMatrixAddr, double *pRet, double *pCols,
             double *pB)
{
  BigMatrix *pMat = (BigMatrix*)R_ExternalPtrAddr(bigMatrixAddr);
  if (pMat->separated_columns()) {
    SepMatrixAccessor<dataT> Mat(*pMat);
    CBinIt1work(Mat[(index_type)pCols[0]-1], pMat->nrow(), pRet, pB);
  } else {
    MatrixAccessor<dataT> Mat(*pMat);
    CBinIt1work(Mat[(index_type)pCols[0]-1], pMat->nrow(), pRet, pB);
  }
}

// ----------------------------------------------------------------

extern "C"
{

SEXP CBinItmain2(SEXP matType, SEXP bigMatrixAddr, SEXP col,
                SEXP breaks1, SEXP breaks2)
{
  int mt = INTEGER_VALUE(matType);
  double *pCols = NUMERIC_DATA(col);
  double *pB1 = NUMERIC_DATA(breaks1);
  double *pB2 = NUMERIC_DATA(breaks2);
  SEXP ret = R_NilValue;
  ret = PROTECT(NEW_NUMERIC((index_type)(pB1[2]*pB2[2])));
  double *pRet = NUMERIC_DATA(ret);
  switch (mt) {
    case 1: {
        CBinIt2<char>(bigMatrixAddr, pRet, pCols, pB1, pB2);
      } break;
    case 2: {
        CBinIt2<short>(bigMatrixAddr, pRet, pCols, pB1, pB2);
      } break;
    case 4: {
        CBinIt2<int>(bigMatrixAddr, pRet, pCols, pB1, pB2);
      } break;
    case 8: {
        CBinIt2<double>(bigMatrixAddr, pRet, pCols, pB1, pB2);
      } break;
  }
  UNPROTECT(1);
  return(ret);
}

SEXP CBinItmain1(SEXP matType, SEXP bigMatrixAddr, SEXP col,
                SEXP breaks)
{
  int mt = INTEGER_VALUE(matType);
  double *pCols = NUMERIC_DATA(col);
  double *pB = NUMERIC_DATA(breaks);
  SEXP ret = R_NilValue;
  ret = PROTECT(NEW_NUMERIC((index_type)pB[2]));
  double *pRet = NUMERIC_DATA(ret);
  switch (mt) {
    case 1: {
        CBinIt1<char>(bigMatrixAddr, pRet, pCols, pB);
      } break;
    case 2: {
        CBinIt1<short>(bigMatrixAddr, pRet, pCols, pB);
      } break;
    case 4: {
        CBinIt1<int>(bigMatrixAddr, pRet, pCols, pB);
      } break;
    case 8: {
        CBinIt1<double>(bigMatrixAddr, pRet, pCols, pB);
      } break;
  }
  UNPROTECT(1);
  return(ret);
}

} // extern "C"
