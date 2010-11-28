
extern "C"
{
  void dgemm (char *, char *, long long *, long long *,
	       long long *, double *, double *, long long *,
	       double *, long long *, double *, double *,
	       long long *);

  void dcopy (long long *, double *, long long *, double *,
	       long long *);

  void dscal (long long *, double *, double *, long long *);

  void daxpy (long long *, double *, double *, long long *,
	       double *, long long *);

  void dgeev (char *, char *, long long *, double *, long long *, 
               double *, double *, double *, long long *, double *,
               long long *, double *, long long *, long long *);

  void dpotrf (char *, long long *, double *, long long *, long long *);

  void dgeqrf (long long *, long long *, double *, long long *, double *,
                double *, long long *, long long *);

  void dgesdd (char *, long long *, long long *, double *, long long *,
                double *, double *, long long *, double *,
                long long *, double *, long long *, long long *,
                long long *);
}
