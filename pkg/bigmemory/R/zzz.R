# ###########
# FILE: zzz.R
#
# We make use of several global options:
#   - bigmemory.print.warning avoid the printing of something massive by default.
#   - bigmemory.typecast.warning alerts the user when something like a numeric
#     value is typecast down to an int, short, or char.
#   - bigmemory.allow.dimnames, FALSE by default, blocks the changing of
#     the dimnames attribute, unless the use explicitly wants to allow it.
#     Note that the dimnames are not in shared memory.

.onLoad <- function(libname, pkgname) {
    library.dynam("bigmemory", pkgname, libname);
    options(bigmemory.print.warning=TRUE)
    options(bigmemory.typecast.warning=TRUE)
    options(bigmemory.allow.dimnames=FALSE)
    cat("\nLoading bigmemory version >= 4.0, a major revision since 3.12.\n")
    cat("If you experience problems moving from 3.12 up to 4.X.Y, please email\n")
    cat("bigmemoryauthors@gmail.com.\n\n")
    cat("Note: bigmemory >= 4.0 no longer includes summary statistics,\n")
    cat("kmeans, and linear regression functionality.  The new packages\n")
    cat("biganalytics, synchronicity, bigalgebra, and bigtabulate provide extended\n")
    cat("functionality.  Further information is available at\n")
    cat("http://www.bigmemory.org.\n\n")
}

#.noGenerics <- TRUE           # This was a problem, not used.

.onUnload <- function(libpath) {
    library.dynam.unload("bigmemory", libpath);
    options(bigmemory.print.warning=NULL)
    options(bigmemory.typecast.warning=NULL)
    options(bigmemory.allow.dimnames=NULL)
}
