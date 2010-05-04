#include <iostream>
#include "ptr_util.hpp"
#include "blas.hpp"

using namespace std;
// Wrappers for miscellaneous BLAS and LAPACK routines.

extern "C"
{
// The R/C interface functions.

  void dgemm_wrapper (SEXP TRANSA, SEXP TRANSB, SEXP M, SEXP N, SEXP K, SEXP ALPHA,
	      SEXP A, SEXP LDA, SEXP B, SEXP LDB, SEXP BETA, SEXP C, SEXP LDC,
	      SEXP A_isBM, SEXP B_isBM, SEXP C_isBM, SEXP C_offset)
  {
/*XXX j should be pointer-sized */
    int j = *(INTEGER_DATA (C_offset));
    double *pA = make_double_ptr (A, A_isBM);
    double *pB = make_double_ptr (B, B_isBM);
    double *pC = make_double_ptr (C, C_isBM) + j;
#ifdef INT64
    long MM = (long)*(DOUBLE_DATA (M));
    long NN = (long)*(DOUBLE_DATA (N));
    long KK = (long)*(DOUBLE_DATA (K));
    long LDAA = (long)*(DOUBLE_DATA (LDA));
    long LDBB = (long)*(DOUBLE_DATA (LDB));
    long LDCC = (long)*(DOUBLE_DATA (LDC));
#else
    int MM = (int)*(DOUBLE_DATA (M));
    int NN = (int)*(DOUBLE_DATA (N));
    int KK = (int)*(DOUBLE_DATA (K));
    int LDAA = (int)*(DOUBLE_DATA (LDA));
    int LDBB = (int)*(DOUBLE_DATA (LDB));
    int LDCC = (int)*(DOUBLE_DATA (LDC));
#endif
      dgemm_ ((char *)CHARACTER_VALUE (TRANSA), 
              (char *)CHARACTER_VALUE (TRANSB),
	      &MM, &NN, &KK, NUMERIC_DATA (ALPHA), pA, &LDAA, pB,
	      &LDBB, NUMERIC_DATA (BETA), pC, &LDCC);
  }
/*
  void dcopy_wrapper (SEXP N, SEXP X, SEXP INCX, SEXP Y, SEXP INCY, SEXP X_isBM,
	      SEXP Y_isBM)
  {
    double *pX = make_double_ptr (X, X_isBM);
    double *pY = make_double_ptr (Y, Y_isBM);
    dcopy_ (INDEX (N), pX, INDEX (INCX), pY,
	    INDEX (INCY));
  }

  void dscal_wrapper (SEXP N, SEXP ALPHA, SEXP Y, SEXP INCY, SEXP Y_isBM)
  {
    double *pY = make_double_ptr (Y, Y_isBM);
    dscal_ (INDEX (N), NUMERIC_DATA (ALPHA), pY, INDEX (INCY));
  }

  void daxpy_wrapper (SEXP N, SEXP ALPHA, SEXP X, SEXP INCX, SEXP Y, SEXP INCY,
	      SEXP X_isBM, SEXP Y_isBM)
  {
    double *pX = make_double_ptr (X, X_isBM);
    double *pY = make_double_ptr (Y, Y_isBM);
    daxpy_ (INDEX (N), NUMERIC_DATA (ALPHA), pX, INDEX (INCX),
	    pY, INDEX (INCY));
  }

  void dgeqrf_wrapper (SEXP M, SEXP N, SEXP A, SEXP LDA, SEXP TAU, SEXP WORK,
	       SEXP LWORK, SEXP INFO, SEXP A_isBM, SEXP TAU_isBM,
	       SEXP WORK_isBM)
  {
    double *pA = make_double_ptr (A, A_isBM);
    double *pTAU = make_double_ptr (TAU, TAU_isBM);
    double *pWORK = make_double_ptr (WORK, WORK_isBM);
    dgeqrf_ (INDEX (M), INDEX (N), pA, INDEX (LDA),
	     pTAU, pWORK, INDEX (LWORK), INDEX (INFO));
  }

  void dpotrf_wrapper (SEXP UPLO, SEXP N, SEXP A, SEXP LDA, SEXP INFO, SEXP A_isBM)
  {
    double *pA = make_double_ptr (A, A_isBM);
    dpotrf_ (CHARACTER_VALUE (UPLO), INDEX (N), pA, INDEX (LDA),
	     INDEX (INFO));
  }

  void dgeev_wrapper (SEXP JOBVL, SEXP JOBVR, SEXP N, SEXP A, SEXP LDA, SEXP WR,
	      SEXP WI, SEXP VL, SEXP LDVL, SEXP VR, SEXP LDVR, SEXP WORK,
	      SEXP LWORK, SEXP INFO, SEXP A_isBM, SEXP WR_isBM, SEXP WI_isBM,
	      SEXP VL_isBM, SEXP VR_isBM, SEXP WORK_isBM)
  {
    double *pA = make_double_ptr (A, A_isBM);
    double *pWR = make_double_ptr (WR, WR_isBM);
    double *pWI = make_double_ptr (WI, WI_isBM);
    double *pVL = make_double_ptr (VL, VL_isBM);
    double *pVR = make_double_ptr (VR, VR_isBM);
    double *pWORK = make_double_ptr (WORK, WORK_isBM);
    dgeev_ (CHARACTER_VALUE (JOBVL), CHARACTER_VALUE (JOBVR),
	    INDEX (N), pA, INDEX (LDA), pWR, pWI, pVL,
	    INDEX (LDVL), pVR, INDEX (LDVR), pWORK,
	    INDEX (LWORK), INDEX (INFO));
  }
*/

/*
  void dgesdd_wrapper (SEXP JOBZ, SEXP M, SEXP N, SEXP A, SEXP LDA, SEXP S, SEXP U,
	       SEXP LDU, SEXP VT, SEXP LDVT, SEXP WORK, SEXP LWORK,
	       SEXP IWORK, SEXP INFO, SEXP A_isBM, SEXP S_isBM, SEXP U_isBM,
	       SEXP VT_isBM, SEXP WORK_isBM, SEXP IWORK_isBM)
  {
    double *pA = make_double_ptr (A, A_isBM);
    double *pS = make_double_ptr (S, S_isBM);
    double *pU = make_double_ptr (U, U_isBM);
    double *pVT = make_double_ptr (VT, VT_isBM);
    double *pWORK = make_double_ptr (WORK, WORK_isBM);
    int *pIWORK = make_int_ptr (IWORK, IWORK_isBM);
    dgesdd_ (CHARACTER_VALUE (JOBZ), INDEX (M), INDEX (N), pA,
	     INDEX (LDA), pS, pU, INDEX (LDU), pVT,
	     INDEX (LDVT), pWORK, INDEX (LWORK), pIWORK,
	     INDEX (INFO));
  }
*/

}
