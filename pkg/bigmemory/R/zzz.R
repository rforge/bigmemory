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
    library.dynam("bigmemory", pkgname, libname)
    options(bigmemory.print.warning=TRUE)
    options(bigmemory.typecast.warning=TRUE)
    options(bigmemory.allow.dimnames=FALSE)
    options(bigmemory.default.type="double")
    packageStartupMessage("\nbigmemory >= 4.0 is a major revision since 3.1.2; please see package\nbiganalytics and http://www.bigmemory.org for more information.\n")
}

#.noGenerics <- TRUE           # This was a problem, not used.

.onUnload <- function(libpath) {
    library.dynam.unload("bigmemory", libpath);
    options(bigmemory.print.warning=NULL)
    options(bigmemory.typecast.warning=NULL)
    options(bigmemory.allow.dimnames=NULL)
    options(bigmemory.default.type=NULL)
}
