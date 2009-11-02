\name{biganalytics-package}
\alias{biganalytics-package}
\alias{biganalytics}
\docType{package}
\title{
biganalytics: A library of utilities for big.matrix objects of package bigmemory.
}
\description{
This package extends the bigmemory package with various analytics.
}
\details{
\tabular{ll}{
Package: \tab biganalytics\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-06-02\cr
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
\seealso{For example,
\code{\link[big.matrix:bigmemory]{big.matrix}}, \code{\link[biglm:biglm-package]{biglm}}
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
