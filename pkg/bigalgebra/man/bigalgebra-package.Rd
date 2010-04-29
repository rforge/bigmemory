\name{bigalgebra-package}
\Rdversion{1.1}
\alias{bigalgebra-package}
\alias{bigalgebra}
\docType{package}
\title{
BLAS and LAPACK routines for native R matrices and
big.matrix objects.
}
\description{
BLAS and LAPACK functionality for native \R matrices and
\code{\link[bigmemory]{big.matrix}} objects.
}
\details{
\tabular{ll}{
Package: \tab bigalgebra\cr
Type: \tab Package\cr
Version: \tab 0.5\cr
Date: \tab 2010-04-25\cr
License: \tab LGPL-3\cr
Copyright: \tab (C) 2010 Michael J. Kane, John W. Emerson, and Bryan Lewis\cr
URL: \tab http://www.bigmemory.org\cr
LazyLoad: \tab yes\cr
}
BLAS and LAPACK functionality for native \R matrices and
\code{\link[bigmemory]{big.matrix}} objects.  These are particularly memory-efficient
and introduce side-effects: calling a function will result in a change of one or more
of the arguments.
}
\author{
Michael J. Kane, Bryan Lewis, John W. Emerson

Maintainer: Mike <bigmemoryauthors@gmail.com>
}
\references{
\url{http://www.netlib.org/blas/}

\url{http://www.netlib.org/lapack/}
}
\keyword{package}
\seealso{
\code{\link[bigmemory]{bigmemory}}, \code{\link[bigmemory]{big.matrix}} 
}
\examples{

# Testing the development of the user-friendly operators:
# if you have any problems, please email us!  - Jay & Mike 4/29/2010

library(bigmemory)
x <- big.matrix(3,4,init=1)
y <- big.matrix(3,4,init=2)
z <- matrix(as.numeric(1:12), 3, 4)
a <- x+y
x[,]
y[,]
a
a[,]

b <- x+z
x[,]
z[,]
b
b[,]

c <- z+y
z[,]
y[,]
c
c[,]

d <- 1 + x
x[,]
d
d[,]

e <- x + (-99)
x[,]
e
e[,]

# Not yet implemented:
#f <- x - 99
#x[,]
#f
#f[,]

g <- +x
g
g[,]

}
