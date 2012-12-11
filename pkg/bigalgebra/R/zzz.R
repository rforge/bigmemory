.onLoad <- function(libname, pkgname) {
  library.dynam("bigalgebra", pkgname, libname);
  options(bigalgebra.temp_pattern="matrix_")
  options(bigalgebra.mixed_airthmetic_returns_R_matrix=TRUE)
  options(bigalgebra.tempdir=tempdir)
}

#.noGenerics <- TRUE

.onUnload <- function(libpath) {
  library.dynam.unload("bigalgebra", libpath);
  options(bigalgebra.temp_pattern=c())
  options(bigalgebra.mixed_airthmetic_returns_R_matrix=c())
  options(bigalgebra.tempdir=c())
}

