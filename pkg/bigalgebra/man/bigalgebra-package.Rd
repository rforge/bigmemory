\name{bigalgebra-package}
\Rdversion{0.7}
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
Version: \tab 0.7\cr
Date: \tab 2010-04-30\cr
License: \tab LGPL-3\cr
Copyright: \tab (C) 2010 Michael J. Kane, John W. Emerson, and Bryan Lewis\cr
URL: \tab http://www.bigmemory.org\cr
LazyLoad: \tab yes\cr
}
BLAS and LAPACK functionality for native \R matrices and
\code{\link[bigmemory]{big.matrix}} objects.  The low-level functions are particularly memory-efficient
and introduce side-effects: calling a function will result in a change of one or more
of the arguments.  The user-friendly methods (e.g. '+', etc...) are non-descructive (no
side-effects), conforming to traditions of the \R environment.

Note that \code{bigalgebra} will be built, by default, with \R's BLAS libraries, which
are 32-bit.  We are working to determine the friendliest way of providing 64-bit support,
which will require a little extra work based on the user's environment.  Feel free to
contact us for more information.
}
\author{
Michael J. Kane, Bryan Lewis, John W. Emerson

Maintainers: Mike and Jay <bigmemoryauthors@gmail.com>
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
a[,]
a <- x-y
a[,]

b <- x+z
b[,]
b <- x-z
b[,]

c <- z+y
c[,]
c <- z-y
c[,]

d <- 1 + x
d[,]
d <- 1 - x
d[,]

e <- x + (-99)
e[,]
e <- x - 99
e[,]

f <- -x
f[,]

g <- +x
g[,]

# This would be allowed with R linear algebra, but is not, here.  For large
# objects, the overhead is substantial and cumbersome, given that similar
# dimensionality mismatches are not supported with assignments.  We don't
# like this, anyway.
#h <- x + c(1,2,3,4,5)

h <- x + c(1,2,3,4)
h[,]
h <- x - c(1,2,3,4)
h[,]
h <-  c(1,2,3,4) - x
h[,]

}
