
.onLoad <- function(libname, pkgname) {
    library.dynam("bigvideo", pkgname, libname)
}

#.noGenerics <- TRUE

.onUnload <- function(libpath) {
    library.dynam.unload("bigvideo", libpath)
}
