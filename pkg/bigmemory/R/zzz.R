# ###########
# FILE: zzz.R
#
# We make use of several global options:
#   - bigmemory.rlocked is used for subtle reasons, having to do most
#     likely with an assignment having a nested mwhich, such as
#
#       x[mwhich(...),] <- something
#
#     Here, a write lock is obtained, but then mwhich will try to get
#     a read lock (and fail).  We use the bigmemory.rlocked option
#     to avoid this conflict. There are probably other similar examples.
#   - bigmemory.print.warning avoid the printing of something massive by default.
#   - bigmemory.typecast.warning alerts the user when something like a numeric
#     value is typecast down to an int, short, or char.

.onLoad <- function(libname, pkgname) {
    library.dynam("bigmemory", pkgname, libname);
    options(bigmemory.print.warning=TRUE)
    options(bigmemory.typecast.warning=TRUE)
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
}
