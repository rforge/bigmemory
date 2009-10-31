# ###########
# FILE: zzz.R
#
# We make use of several global options:
#   - bigmemory.print.warning avoid the printing of something massive by default.
#   - bigmemory.typecast.warning alerts the user when something like a numeric
#     value is typecast down to an int, short, or char.

.onLoad <- function(libname, pkgname) {
    library.dynam("bigmemory", pkgname, libname);
    options(bigmemory.print.warning=TRUE)
    options(bigmemory.typecast.warning=TRUE)
    options(bigmemory.allow.dimnames=FALSE)
    cat("\nLoading bigmemory version >=4.0.\n\n")
    cat("Note that bigmemory >= 4.0 no longer includes advanced\n")
    cat("functionality.  New packages biganalytics, synchronicity,\n")
    cat("and bigalgebra provide extended functionality.  Further\n")
    cat("information is available at http://www.bigmemory.org.\n\n")
}

#.noGenerics <- TRUE           # This was a problem, not used.

.onUnload <- function(libpath) {
    library.dynam.unload("bigmemory", libpath);
    options(bigmemory.print.warning=NULL)
    options(bigmemory.typecast.warning=NULL)
    options(bigmemory.allow.dimnames=NULL)
}
