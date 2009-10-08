.onLoad <- function(libname, pkgname) {
    library.dynam("bigalgebra", pkgname, libname);
}

#.noGenerics <- TRUE

.onUnload <- function(libpath) {
    library.dynam.unload("bigalgebra", libpath);
}

