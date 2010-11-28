#include "ptr_util.hpp"
#include "blas.hpp"

// Wrappers for miscellaneous BLAS and LAPACK routines.

extern "C"
{
  void dgemm_wrapper (SEXP TRANSA, SEXP TRANSB, SEXP M, SEXP N, SEXP K, 
              SEXP ALPHA, SEXP A, SEXP LDA, SEXP B, SEXP LDB, SEXP BETA, 
              SEXP C, SEXP LDC, SEXP A_isBM, SEXP B_isBM, SEXP C_isBM, 
              SEXP C_offset)
  {
    long j = *(DOUBLE_DATA (C_offset));
    double *pA = make_double_ptr (A, A_isBM);
    double *pB = make_double_ptr (B, B_isBM);
    double *pC = make_double_ptr (C, C_isBM) + j;
    INT MM = (INT)*(DOUBLE_DATA (M));
    INT NN = (INT)*(DOUBLE_DATA (N));
    INT KK = (INT)*(DOUBLE_DATA (K));
    INT LDAA = (INT)*(DOUBLE_DATA (LDA));
    INT LDBB = (INT)*(DOUBLE_DATA (LDB));
    INT LDCC = (INT)*(DOUBLE_DATA (LDC));
#ifdef ACMLBLAS
      dgemm (*((char *)CHARACTER_VALUE (TRANSA)),
              *((char *)CHARACTER_VALUE (TRANSB)),
	      MM, NN, KK, *(NUMERIC_DATA (ALPHA)), pA, LDAA, pB,
	      LDBB, *(NUMERIC_DATA (BETA)), pC, LDCC);
#else
      dgemm ((char *)CHARACTER_VALUE (TRANSA), 
              (char *)CHARACTER_VALUE (TRANSB),
	      &MM, &NN, &KK, NUMERIC_DATA (ALPHA), pA, &LDAA, pB,
	      &LDBB, NUMERIC_DATA (BETA), pC, &LDCC);
#endif
  }

  void dcopy_wrapper (SEXP N, SEXP X, SEXP INCX, SEXP Y, SEXP INCY, SEXP X_isBM,
	      SEXP Y_isBM)
  {
    double *pX = make_double_ptr (X, X_isBM);
    double *pY = make_double_ptr (Y, Y_isBM);
    INT NN = (INT)*(DOUBLE_DATA (N));
    INT INCXX = (INT)*(DOUBLE_DATA (INCX));
    INT INCYY = (INT)*(DOUBLE_DATA (INCY));
#ifdef ACMLBLAS
    dcopy (NN, pX, INCXX, pY, INCYY);
#else
    dcopy (&NN, pX, &INCXX, pY, &INCYY);
#endif
  }

  void dscal_wrapper (SEXP N, SEXP ALPHA, SEXP Y, SEXP INCY, SEXP Y_isBM)
  {
    double *pY = make_double_ptr (Y, Y_isBM);
    INT NN = (INT)*(DOUBLE_DATA (N));
    INT INCYY = (INT)*(DOUBLE_DATA (INCY));
#ifdef ACMLBLAS
    dscal (NN, *(NUMERIC_DATA (ALPHA)), pY, INCYY);
#else
    dscal (&NN, NUMERIC_DATA (ALPHA), pY, &INCYY);
#endif
  }

  void daxpy_wrapper (SEXP N, SEXP ALPHA, SEXP X, SEXP INCX, SEXP Y, SEXP INCY,
	      SEXP X_isBM, SEXP Y_isBM)
  {
    double *pX = make_double_ptr (X, X_isBM);
    double *pY = make_double_ptr (Y, Y_isBM);
    INT NN = (INT)*(DOUBLE_DATA (N));
    INT INCXX = (INT)*(DOUBLE_DATA (INCX));
    INT INCYY = (INT)*(DOUBLE_DATA (INCY));
#ifdef ACMLBLAS
    daxpy (NN, *(NUMERIC_DATA (ALPHA)), pX, INCXX, pY, INCYY);
#else
    daxpy (&NN, NUMERIC_DATA (ALPHA), pX, &INCXX, pY, &INCYY);
#endif
  }

  void dgeqrf_wrapper (SEXP M, SEXP N, SEXP A, SEXP LDA, SEXP TAU, SEXP WORK,
	       SEXP LWORK, SEXP INFO, SEXP A_isBM, SEXP TAU_isBM,
	       SEXP WORK_isBM)
  {
    double *pA = make_double_ptr (A, A_isBM);
    double *pTAU = make_double_ptr (TAU, TAU_isBM);
    double *pWORK = make_double_ptr (WORK, WORK_isBM);
    INT MM = (INT)*(DOUBLE_DATA (M));
    INT NN = (INT)*(DOUBLE_DATA (N));
    INT LDAA = (INT)*(DOUBLE_DATA (LDA));
    INT LWORKK = (INT)*(DOUBLE_DATA (LWORK));
    INT INFOO = (INT)*(DOUBLE_DATA (INFO));
#ifdef ACMLBLAS
    dgeqrf (MM, NN, pA, LDAA, pTAU, &INFOO);
#else
    dgeqrf (&MM, &NN, pA, &LDAA, pTAU, pWORK, &LWORKK, &INFOO);
#endif
  }

  void dpotrf_wrapper (SEXP UPLO, SEXP N, SEXP A, SEXP LDA, 
                       SEXP INFO, SEXP A_isBM)
  {
    double *pA = make_double_ptr (A, A_isBM);
    INT NN = (INT)*(DOUBLE_DATA (N));
    INT LDAA = (INT)*(DOUBLE_DATA (LDA));
    INT INFOO = (INT)*(DOUBLE_DATA (INFO));
#ifdef ACMLBLAS
    dpotrf (*((char *)CHARACTER_VALUE (UPLO)), NN, pA, LDAA, &INFOO);
#else
    dpotrf ((char *)CHARACTER_VALUE (UPLO), &NN, pA, &LDAA, &INFOO);
#endif
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
    INT NN = (INT)*(DOUBLE_DATA (N));
    INT LDAA = (INT)*(DOUBLE_DATA (LDA));
    INT LDVLL = (INT)*(DOUBLE_DATA (LDVL));
    INT LDVRR = (INT)*(DOUBLE_DATA (LDVR));
    INT LWORKK = (INT)*(DOUBLE_DATA (LWORK));
    INT INFOO = (INT)*(DOUBLE_DATA (INFO));
#ifdef ACMLBLAS
    dgeev (*((char *)CHARACTER_VALUE (JOBVL)),
           *((char *)CHARACTER_VALUE (JOBVR)),
	   NN, pA, LDAA, pWR, pWI, pVL, LDVLL, pVR, LDVRR, &INFOO);
#else
    dgeev ((char *)CHARACTER_VALUE (JOBVL), (char *)CHARACTER_VALUE (JOBVR),
	    &NN, pA, &LDAA, pWR, pWI, pVL, &LDVLL, pVR, &LDVRR, pWORK,
	    &LWORKK, &INFOO);
#endif
  }

  void dgesdd_wrapper (SEXP JOBZ, SEXP M, SEXP N, SEXP A, SEXP LDA, 
               SEXP S, SEXP U,
	       SEXP LDU, SEXP VT, SEXP LDVT, SEXP WORK, SEXP LWORK,
	       SEXP INFO, SEXP A_isBM, SEXP S_isBM, SEXP U_isBM,
	       SEXP VT_isBM, SEXP WORK_isBM)
  {
    INT *pIWORK;
    INT piworkdim;
    double *pA = make_double_ptr (A, A_isBM);
    double *pS = make_double_ptr (S, S_isBM);
    double *pU = make_double_ptr (U, U_isBM);
    double *pVT = make_double_ptr (VT, VT_isBM);
    double *pWORK = make_double_ptr (WORK, WORK_isBM);
    INT MM = (INT)*(DOUBLE_DATA (N));
    INT NN = (INT)*(DOUBLE_DATA (N));
    INT LDAA = (INT)*(DOUBLE_DATA (LDA));
    INT LDUU = (INT)*(DOUBLE_DATA (LDU));
    INT LDVTT = (INT)*(DOUBLE_DATA (LDVT));
    INT LWORKK = (INT)*(DOUBLE_DATA (LWORK));
    INT INFOO = (INT)*(DOUBLE_DATA (INFO));

    piworkdim = 8*MM;
    if(NN>MM) piworkdim = 8*NN;
#ifdef ACMLBLAS
    dgesdd (*((char *)CHARACTER_VALUE (JOBZ)), MM, NN, pA,
	     LDAA, pS, pU, LDUU, pVT, LDVTT, &INFOO);
#else
    pIWORK = (INT *)malloc(piworkdim*sizeof(INT));
    dgesdd ((char *)CHARACTER_VALUE (JOBZ), &MM, &NN, pA,
	     &LDAA, pS, pU, &LDUU, pVT,
	     &LDVTT, pWORK, &LWORKK, pIWORK, &INFOO);
    free(pIWORK);
#endif
  }

}
