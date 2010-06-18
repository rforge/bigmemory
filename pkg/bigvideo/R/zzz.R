
.onLoad <- function(libname, pkgname) {
  library.dynam("bigvideo", pkgname, libname)

  packageStartupMessage("\nbigvideo is a work in progress (June 2010); we encourage you to try\nand appreciate feedback, but haven't even started to document it and\nreserve the right to change it.  It should not be considered stable.\nBut have fun playing around with us!\n")

}

#.noGenerics <- TRUE

.onUnload <- function(libpath) {
    library.dynam.unload("bigvideo", libpath)
}
