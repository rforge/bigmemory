
extern "C"
{
  void dgemm_ (const char *, const char *, const int *, const int *,
               const int *, const double *, const double *, const int *,
               const double *, const int *, const double *, double *,
               const int *);

  void dcopy_ (const int *, const double *, const int *, double *,
               const int *);

  void dscal_ (const int *, const double *, double *, const int *);

  void daxpy_ (const int *, const double *, const double *, const int *,
               double *, const int *);
}
