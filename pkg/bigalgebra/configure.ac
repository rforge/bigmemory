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

# BLAS in Alpha CXML library?
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(cxml, $sgemm, [acx_blas_ok=yes;BLAS_LIBS="-lcxml"])
fi

# BLAS in Alpha DXML library? (now called CXML, see above)
if test $acx_blas_ok = no; then
	AC_CHECK_LIB(dxml, $sgemm, [acx_blas_ok=yes;BLAS_LIBS="-ldxml"])
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

dnl ADD ACML AND INTEL MKL HERE!!!

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

AC_ARG_WITH(lapackInclude,
  [AC_HELP_STRING([--with-lapackInclude="header(s)"], [use custom LAPACK header])])
AC_ARG_WITH(blasInclude,
  [AC_HELP_STRING([--with-blasInclude="header(s)"], [use custom BLAS header])])
if test -z "${with_lapackInclude}"; then
  LAPACKHEADER="\"R_ext/Lapack.h\""
else
  LAPACKHEADER="${with_lapackInclude}"
fi
if test -z "${with_blasInclude}"; then
  BLASHEADER="\"refblas.h\""
else
  BLASHEADER="${with_blasInclude}"
fi

echo "LAPACKHEADER $LAPACKHEADER"

ACX_BLAS([LIBS="$LAPACK_LIBS $BLAS_LIBS $LIBS $FLIBS $RLIB"],[AC_MSG_NOTICE([could not find your BLAS library])])
BM=`echo 'cat(file.path(.libPaths(), "bigmemory", "libs","bigmemory.so")[[1]])' | ${R_HOME}/bin/R --slave`
BI=`echo 'cat(file.path(.libPaths(), "bigmemory", "include")[[1]])' | ${R_HOME}/bin/R --slave`
BIGALGEBRA_CPPFLAGS="-I${BI} $CXXFLAGS $BIGALGEBRA_CPPFLAGS"
LIBS="$BM $LIBS"
echo "USING LAPACK_LIBS = $LAPACK_LIBS"
echo "USING BLAS_LIBS = $BLAS_LIBS"

AC_SUBST(BIGALGEBRA_CPPFLAGS)
AC_SUBST(LIBS)
AC_SUBST(BLASHEADER)
AC_SUBST(LAPACKHEADER)
AC_CONFIG_FILES([src/Makevars])
AC_CONFIG_FILES([src/blas.hpp])
AC_OUTPUT
