
extern "C"
{
  void dgemm_ (char *, char *, long long *, long long *,
	       long long *, double *, double *, long long *,
	       double *, long long *, double *, double *,
	       long long *);

  void dcopy_ (long long *, double *, long long *, double *,
	       long long *);

  void dscal_ (long long *, double *, double *, long long *);

  void daxpy_ (long long *, double *, double *, long long *,
	       double *, long long *);
}
