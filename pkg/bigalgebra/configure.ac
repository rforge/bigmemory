AC_INIT([bigalgera], [0.6])
R_HOME=`R RHOME`
echo "R_HOME IS ${R_HOME}"
if test -z "${R_HOME}"; then
  echo "could not determine R_HOME"
  exit 1
fi
CC=`"${R_HOME}/bin/R" CMD config CC`
CPP=`"${R_HOME}/bin/R" CMD config CPP`
F77=`"${R_HOME}/bin/R" CMD config F77`
AC_PROG_CC
AC_PROG_CPP
AC_PROG_F77

FLIBS=`"${R_HOME}/bin/R" CMD config FLIBS`
CXXFLAGS="`"${R_HOME}/bin/R" CMD config --cppflags` ${CXXFLAGS}"
RLIB=`"${R_HOME}/bin/R" CMD config --ldflags`

BLAS_LIBS=""
AC_ARG_WITH(blas,
	[AC_HELP_STRING([--with-blas=<lib>], [use BLAS library <lib>])])
case $with_blas in
	-* | */* | *.a | *.so | *.so.* | *.o) BLAS_LIBS="$with_blas" ;;
	*) BLAS_LIBS="-l$with_blas" ;;
esac
LAPACK_LIBS=""
AC_ARG_WITH(lapack,
	[AC_HELP_STRING([--with-lapack=<lib>], [use LAPACK library <lib>])])
case $with_lapack in
	-* | */* | *.a | *.so | *.so.* | *.o) LAPACK_LIBS="$with_lapack" ;;
	*) LAPACK_LIBS="-l$with_lapack" ;;
esac
if test "$with_blas" == ""; then
  with_blas="refblas64"
fi
if test "$with_blas" == "refblas64"; then
  LAPACK_LIBS="-L. -L./src/ -L./src/refblas64/ -L./refblas64/ -lrefblas64"
  BLAS_LIBS="-L. -L./src/ -L./src/refblas64/ -L./refblas64/ -lrefblas64"
  make  -C src/refblas64
fi
if test "${LAPACK_LIBS}" == "-l"; then
  LAPACK_LIBS="-L. -L./src/ -L./src/refblas64/ -L./refblas64/ -lrefblas64"
fi

AC_ARG_WITH(int64,
  AC_HELP_STRING([--with-int64=type],[Specify 64-bit integer indexing type]),
       [INT64="#define INT ${with_int64}"], [INT64="#define INT long long"])

AC_ARG_WITH(incDir,
  [AC_HELP_STRING([--with-incDir=<dirs>], [Set extra include dirs])])
if test -n "${with_incDir}"; then
  CXXFLAGS="${with_incDir} ${CXXFLAGS}"
fi

AC_ARG_WITH(lapackHeader,
  [AC_HELP_STRING([--with-lapackHeader="header"], [use custom LAPACK header])])
AC_ARG_WITH(blasHeader,
  [AC_HELP_STRING([--with-blasHeader="header"], [use custom BLAS header])])
if test -z "${with_lapackHeader}"; then
  LAPACKHEADER="\"refblas64longlong.h\""
else
  LAPACKHEADER="\"${with_lapackHeader}\""
  echo "Using LAPACK header files $LAPACKHEADER"
fi
if test -z "${with_blasHeader}"; then
  BLASHEADER="\"refblas64longlong.h\""
else
  BLASHEADER="\"${with_blasHeader}\""
  echo "Using BLAS header files $BLASHEADER"
fi

echo "Using LAPACK_LIBS $LAPACK_LIBS"
echo "Using BLAS_LIBS $BLAS_LIBS"

LIBS="$LAPACK_LIBS $BLAS_LIBS $LIBS $FLIBS $RLIB"

# Check for AMD ACML libary
AC_F77_FUNC(acmlinfo)
AC_MSG_CHECKING([for $acmlinfo in $BLAS_LIBS])
AC_TRY_LINK_FUNC($acmlinfo, [ACML="#define ACMLBLAS"],[ACML=""])

#BM=`echo 'cat(file.path(.libPaths(), "bigmemory", "libs","bigmemory.so")[[1]])' | ${R_HOME}/bin/R --slave`
#BI=`echo 'cat(file.path(.libPaths(), "bigmemory", "include")[[1]])' | ${R_HOME}/bin/R --slave`
#BIGALGEBRA_CPPFLAGS="-I${BI} $CXXFLAGS $BIGALGEBRA_CPPFLAGS"
#LIBS="$BM $LIBS"
BIGALGEBRA_CPPFLAGS="$CXXFLAGS $BIGALGEBRA_CPPFLAGS"
LIBS="$LIBS"

AC_SUBST(BIGALGEBRA_CPPFLAGS)
AC_SUBST(LIBS)
AC_SUBST(BLASHEADER)
AC_SUBST(LAPACKHEADER)
AC_SUBST(INT64)
AC_SUBST(ACML)
AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_FILES([src/blas.hpp])
AC_OUTPUT
