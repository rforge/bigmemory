AC_DEFUN([ACX_BLAS], [
AC_PREREQ(2.50)
AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
acx_blas_ok=no

AC_ARG_WITH(blas,
	[AC_HELP_STRING([--with-blas=<lib>], [use BLAS library <lib>])])
case $with_blas in
	yes | "") ;;
	no) acx_blas_ok=disable ;;
	-* | */* | *.a | *.so | *.so.* | *.o) BLAS_LIBS="$with_blas" ;;
	*) BLAS_LIBS="-l$with_blas" ;;
esac

# Get fortran linker names of BLAS functions to check for.
AC_F77_FUNC(sgemm)
AC_F77_FUNC(dgemm)
AC_F77_FUNC(acmlinfo)

acx_blas_save_LIBS="$LIBS"
LIBS="$LIBS $FLIBS"

# First, check BLAS_LIBS environment variable
if test $acx_blas_ok = no; then
if test "x$BLAS_LIBS" != x; then
	save_LIBS="$LIBS"; LIBS="$BLAS_LIBS $LIBS"
	AC_MSG_CHECKING([for $sgemm in $BLAS_LIBS])
	AC_TRY_LINK_FUNC($sgemm, [acx_blas_ok=yes], [BLAS_LIBS=""])
	AC_MSG_RESULT($acx_blas_ok)
	LIBS="$save_LIBS"
fi
fi

# BLAS linked to by default?  (happens on some supercomputers)
if test $acx_blas_ok = no; then
	save_LIBS="$LIBS"; LIBS="$LIBS"
	AC_CHECK_FUNC($sgemm, [acx_blas_ok=yes])
	LIBS="$save_LIBS"
fi

# BLAS in ATLAS library? (http://math-atlas.sourceforge.net/)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(atlas, ATL_xerbla,
		[AC_CHECK_LIB(f77blas, $sgemm,
		[AC_CHECK_LIB(cblas, cblas_dgemm,
			[acx_blas_ok=yes
			 BLAS_LIBS="-lcblas -lf77blas -latlas"],
			[], [-lf77blas -latlas])],
			[], [-latlas])])
fi

# BLAS in PhiPACK libraries? (requires generic BLAS lib, too)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(blas, $sgemm,
		[AC_CHECK_LIB(dgemm, $dgemm,
		[AC_CHECK_LIB(sgemm, $sgemm,
			[acx_blas_ok=yes; BLAS_LIBS="-lsgemm -ldgemm -lblas"],
			[], [-lblas])],
			[], [-lblas])])
fi


# BLAS in Sun Performance library?
if test $acx_blas_ok = no; then
	if test "x$GCC" != xyes; then # only works with Sun CC
		AC_CHECK_LIB(sunmath, acosp,
			[AC_CHECK_LIB(sunperf, $sgemm,
        			[BLAS_LIBS="-xlic_lib=sunperf -lsunmath"
                                 acx_blas_ok=yes],[],[-lsunmath])])
	fi
fi

# BLAS in SCSL library?  (SGI/Cray Scientific Library)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(scs, $sgemm, [acx_blas_ok=yes; BLAS_LIBS="-lscs"])
fi

# BLAS in SGIMATH library?
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(complib.sgimath, $sgemm,
		     [acx_blas_ok=yes; BLAS_LIBS="-lcomplib.sgimath"])
fi

# BLAS in IBM ESSL library? (requires generic BLAS lib, too)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(blas, $sgemm,
		[AC_CHECK_LIB(essl, $sgemm,
			[acx_blas_ok=yes; BLAS_LIBS="-lessl -lblas"],
			[], [-lblas $FLIBS])])
fi

# Generic BLAS library?
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(blas, $sgemm, [acx_blas_ok=yes; BLAS_LIBS="-lblas"])
fi

AC_SUBST(BLAS_LIBS)

LIBS="$acx_blas_save_LIBS"

# Finally, execute ACTION-IF-FOUND/ACTION-IF-NOT-FOUND:
if test x"$acx_blas_ok" = xyes; then
        ifelse([$1],,AC_DEFINE(HAVE_BLAS,1,[Define if you have a BLAS library.]),[$1])
        :
else
        acx_blas_ok=no
        $2
fi
])dnl ACX_BLAS



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

LAPACK_LIBS=""
AC_ARG_WITH(lapack,
	[AC_HELP_STRING([--with-lapack=<lib>], [use LAPACK library <lib>])])
case $with_lapack in
	-* | */* | *.a | *.so | *.so.* | *.o) LAPACK_LIBS="$with_lapack" ;;
	*) LAPACK_LIBS="-l$with_lapack" ;;
esac
if test "${LAPACK_LIBS}" == "-l"; then
  LAPACK_LIBS=`"${R_HOME}/bin/R" CMD config LAPACK_LIBS`
fi

AC_ARG_WITH(int64,
  AC_HELP_STRING([--with-int64=type],[Specify 64-bit integer indexing type]),
       [INT64="#define INT ${with_int64}"], [INT64="#define INT int"])

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
  LAPACKHEADER="\"R_ext/Lapack.h\""
else
  LAPACKHEADER="\"${with_lapackHeader}\""
  echo "Using LAPACK header files $LAPACKHEADER"
fi
if test -z "${with_blasHeader}"; then
  BLASHEADER="\"refblas.h\""
else
  BLASHEADER="\"${with_blasHeader}\""
  echo "Using BLAS header files $BLASHEADER"
fi

ACX_BLAS([LIBS="$LAPACK_LIBS $BLAS_LIBS $LIBS $FLIBS $RLIB"],[AC_MSG_NOTICE([could not find your BLAS library])])

# Check for AMD ACML libary
AC_MSG_CHECKING([for $acmlinfo in $BLAS_LIBS])
AC_TRY_LINK_FUNC($acmlinfo, [ACML="#define ACMLBLAS"],[ACML=""])

BM=`echo 'cat(system.file(package="bigmemory"))' | ${R_HOME}/bin/R --slave`
BIGALGEBRA_CPPFLAGS="-I${BM}/include $CXXFLAGS $BIGALGEBRA_CPPFLAGS"
LIBS="${BM}/libs/bigmemory.so $LIBS"

echo "Using LAPACK_LIBS $LAPACK_LIBS"
echo "Using BLAS_LIBS $BLAS_LIBS"

AC_SUBST(BIGALGEBRA_CPPFLAGS)
AC_SUBST(LIBS)
AC_SUBST(BLASHEADER)
AC_SUBST(LAPACKHEADER)
AC_SUBST(INT64)
AC_SUBST(ACML)
AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_FILES([src/blas.hpp])
AC_OUTPUT
