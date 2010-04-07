\name{bigtabulate-package}
\alias{bigtabulate-package}
\docType{package}
\title{
bigtabulate: a tapply()-like function for both matrices and big.matrix objects.
}
\description{
This package extends the bigmemory package, but also provides a new function for
use with \R \code{matrix} objects.  The function \code{\link{bigtabulate}}
is like \code{\link{tapply}}, doing tabulations based on a specified conditional
structure.  In other words, for every cell of a (possibly multidimensional)
contingency table, collect some statistic(s) of interest.  At the lowest level,
this supports an extremely fast alternative to \code{\link{table}} for matrices
that also works on \code{\link[bigmemory]{big.matrix}} objects.  But general functions
may be applied, as well.
}
\details{
\tabular{ll}{
Package: \tab bigtabulate\cr
Type: \tab Package\cr
Version: \tab 1.0.4\cr
Date: \tab 2010-04-07\cr
License: \tab LGPL-3\cr
Copyright: \tab (C) 2010 John W. Emerson and Michael J. Kane\cr
URL: \tab http://www.bigmemory.org\cr
LazyLoad: \tab yes\cr
}

The \pkg{bigmemory} package contains the core support and basic summary
functions; \pkg{bigtabulate} contains extended tabulation functionality.

}
\author{
John W. Emerson and Michael J. Kane

Maintainers: Jay and Mike <bigmemoryauthors@gmail.com>
}
\keyword{package}
\references{See \url{http://www.bigmemory.org}.}
\seealso{For example, see
\code{\link[bigmemory]{big.matrix}}, \code{\link[biglm]{biglm}},
\code{\link[biganalytics]{bigkmeans}}, \code{\link[biganalytics]{binit}}, \code{\link[biganalytics]{colmean}}, \code{\link{tapply}}.
}
\examples{
# A basic example:



}
