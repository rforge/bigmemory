\name{biganalytics-package}
\alias{biganalytics-package}
\alias{biganalytics}
\docType{package}
\title{
biganalytics: A library of utilities for big.matrix objects of package bigmemory.
}
\description{
This package extends the bigmemory package with various analytics.  In addition
to the more obvious summary statistics (see \code{\link{colmean}}, etc...),
\pkg{biganalytics} offers \code{\link{biglm.big.matrix}},
\code{\link{bigglm.big.matrix}}, \code{\link{bigkmeans}}, and
\code{\link{binit}}.
}
\details{
\tabular{ll}{
Package: \tab biganalytics\cr
Type: \tab Package\cr
Version: \tab 1.0.2\cr
Date: \tab 2010-03-10\cr
License: \tab LGPL-3\cr
}

The \pkg{bigmemory} package contains the core support and basic summary
functions; \pkg{biganalytics} contains more advanced analytics on
\code{big.matrix} objects.

}
\author{
John W. Emerson and Michael J. Kane

Maintainer: Jay Emerson <john.emerson@yale.edu>
}
\keyword{package}
\references{See \url{http://www.stat.yale.edu/~jay/bigmemory}.}
\seealso{For example, see
\code{\link[bigmemory]{big.matrix}}, \code{\link[biglm]{biglm}},
\code{\link{bigkmeans}}, \code{\link{binit}}, \code{\link{colmean}}.
}
\examples{
# Our examples are all trivial in size, rather than burning huge amounts
# of memory simply to demonstrate the package functionality.

library(bigmemory)

x <- big.matrix(5, 2, type="integer", init=0, 
  dimnames=list(NULL, c("alpha", "beta")))
x
x[,]
x[,1] <- 1:5
x[,]
mean(x)
colmean(x)
summary(x)
}
