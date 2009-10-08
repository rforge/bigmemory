#include "ptr_util.hpp" 
// The BLAS and LAPACK routines.

extern "C"
{

void dgemm_(const char*, const char*, const int*, const int*, const int*, 
  const double*, const double*, const int*, const double*, const int*, 
  const double*, double*, const int*);

void dcopy_(const int*, const double*, const int*, double*, const int*);

void dscal_(const int*, const double*, double*, const int*);

void daxpy_(const int*, const double*, const double*, const int*, double*,
  const int*);

void dgeqrf_(const int*, const int*, double*, const int*, double*,
  double*, const int*, int*);
  
void dpotrf_( const char*, const int*, double*, const int*, const int*);
  
void dgesdd_( const char*, const int*, const int*, double*,
  const int*, double*, double*, const int*, double*, const int*,
  double*, const int*, int*, int*);

void dgeev_( const char*, const char*, const int*, double*, const int*,
  double*, double*, double*, const int*, double*, const int*, double*,
  const int*, int*);

// The R/C interface functions.

void dgemm( SEXP TRANSA, SEXP TRANSB, SEXP M, SEXP N, SEXP K, SEXP ALPHA, 
  SEXP A, SEXP LDA, SEXP B, SEXP LDB, SEXP BETA, SEXP C, SEXP LDC, SEXP A_isBM, 
  SEXP B_isBM, SEXP C_isBM, SEXP C_offset)
{
  int j = *(INTEGER_DATA(C_offset));
  double *pA = make_double_ptr(A, A_isBM);
  double *pB = make_double_ptr(B, B_isBM);
  double *pC = make_double_ptr(C, C_isBM) + j;
  dgemm_( CHARACTER_VALUE(TRANSA), CHARACTER_VALUE(TRANSB), INTEGER_DATA(M),
    INTEGER_DATA(N), INTEGER_DATA(K), NUMERIC_DATA(ALPHA), pA,
    INTEGER_DATA(LDA), pB, INTEGER_DATA(LDB), NUMERIC_DATA(BETA),
    pC, INTEGER_DATA(LDC) );
}

void dcopy( SEXP N, SEXP X, SEXP INCX, SEXP Y, SEXP INCY, SEXP X_isBM,
  SEXP Y_isBM )
{
  double *pX = make_double_ptr(X, X_isBM);
  double *pY = make_double_ptr(Y, Y_isBM);
  dcopy_( INTEGER_DATA(N), pX, INTEGER_DATA(INCX), pY, INTEGER_DATA(INCY) );
}

void dscal( SEXP N, SEXP ALPHA, SEXP Y, SEXP INCY, SEXP Y_isBM )
{
  double *pY = make_double_ptr(Y, Y_isBM);
  dscal_(INTEGER_DATA(N), NUMERIC_DATA(ALPHA), pY, INTEGER_DATA(INCY));
}

void daxpy( SEXP N, SEXP ALPHA, SEXP X, SEXP INCX, SEXP Y, SEXP INCY,
  SEXP X_isBM, SEXP Y_isBM )
{
  double *pX = make_double_ptr(X, X_isBM);
  double *pY = make_double_ptr(Y, Y_isBM);
  daxpy_( INTEGER_DATA(N), NUMERIC_DATA(ALPHA), pX, INTEGER_DATA(INCX),
    pY, INTEGER_DATA(INCY) );
}

void dgeqrf( SEXP M, SEXP N, SEXP A, SEXP LDA, SEXP TAU, SEXP WORK, SEXP LWORK, 
  SEXP INFO, SEXP A_isBM, SEXP TAU_isBM, SEXP WORK_isBM)
{
  double *pA = make_double_ptr(A, A_isBM);
  double *pTAU = make_double_ptr(TAU, TAU_isBM);
  double *pWORK = make_double_ptr(WORK, WORK_isBM);
  dgeqrf_( INTEGER_DATA(M), INTEGER_DATA(N), pA, INTEGER_DATA(LDA),
    pTAU, pWORK, INTEGER_DATA(LWORK), INTEGER_DATA(INFO));
}

void dpotrf( SEXP UPLO, SEXP N, SEXP A, SEXP LDA, SEXP INFO, SEXP A_isBM)
{
  double *pA = make_double_ptr(A, A_isBM);
  dpotrf_( CHARACTER_VALUE(UPLO), INTEGER_DATA(N), pA, INTEGER_DATA(LDA),
    INTEGER_DATA(INFO) );
}

void dgeev( SEXP JOBVL, SEXP JOBVR, SEXP N, SEXP A, SEXP LDA, SEXP WR, SEXP WI, 
  SEXP VL, SEXP LDVL, SEXP VR, SEXP LDVR, SEXP WORK, SEXP LWORK, SEXP INFO, 
  SEXP A_isBM, SEXP WR_isBM, SEXP WI_isBM, SEXP VL_isBM, SEXP VR_isBM,
  SEXP WORK_isBM)
{
  double *pA = make_double_ptr(A, A_isBM);
  double *pWR = make_double_ptr(WR, WR_isBM);
  double *pWI = make_double_ptr(WI, WI_isBM);
  double *pVL = make_double_ptr(VL, VL_isBM);
  double *pVR = make_double_ptr(VR, VR_isBM);
  double *pWORK = make_double_ptr(WORK, WORK_isBM);
  dgeev_( CHARACTER_VALUE(JOBVL), CHARACTER_VALUE(JOBVR), INTEGER_DATA(N), pA,
    INTEGER_DATA(LDA), pWR, pWI, pVL, INTEGER_DATA(LDVL), pVR, 
    INTEGER_DATA(LDVR), pWORK, INTEGER_DATA(LWORK), INTEGER_DATA(INFO) );
}

void dgesdd( SEXP JOBZ, SEXP M, SEXP N, SEXP A, SEXP LDA, SEXP S, SEXP U, 
  SEXP LDU, SEXP VT, SEXP LDVT, SEXP WORK, SEXP LWORK, SEXP IWORK, SEXP INFO,
  SEXP A_isBM, SEXP S_isBM, SEXP U_isBM, SEXP VT_isBM, SEXP WORK_isBM,
  SEXP IWORK_isBM )
{
  double *pA = make_double_ptr(A, A_isBM);
  double *pS = make_double_ptr(S, S_isBM);
  double *pU = make_double_ptr(U, U_isBM);
  double *pVT = make_double_ptr(VT, VT_isBM);
  double *pWORK = make_double_ptr(WORK, WORK_isBM);
  int *pIWORK = make_int_ptr(IWORK, IWORK_isBM);
  dgesdd_(CHARACTER_VALUE(JOBZ), INTEGER_DATA(M), INTEGER_DATA(N), pA,
    INTEGER_DATA(LDA), pS, pU, INTEGER_DATA(LDU), pVT, INTEGER_DATA(LDVT),
    pWORK, INTEGER_DATA(LWORK), pIWORK, INTEGER_DATA(INFO));
}

}
