\name{daxpy}
\Rdversion{1.1}
\alias{daxpy}
\title{BLAS daxpy functionality}
\description{
This function implements the function Y := ALPHA * X + Y where
X and Y may be either native R matrices or \code{\link[bigmemory]{big.matrix}} objects.  
}
\usage{
daxpy(N=NULL, ALPHA=1, X, INCX=1, Y, INCY=1)
}
\arguments{
  \item{N}{the number of elements in the matrix}
  \item{ALPHA}{the value to scale the matrix \code{X} by}
  \item{X}{either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} object}
  \item{INCX}{spacing between elements of \code{X} to be copied}
  \item{Y}{either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} object}
  \item{INCY}{spacing between elements of \code{Y} to be copied}
}
\details{
If \code{N}, \code{INCX}, \code{INCY} are not specified, the default is addition of all
elements of \code{X} to \code{Y}.
}
\value{
The function returns 0 if the operation is successful.
It should be noted that the function mutates \code{Y}.
}
\references{
\url{http://www.netlib.org/blas/daxpy.f}
}
\author{
Michael J. Kane
}

\seealso{
\code{\link[bigmemory]{bigmemory}}
}
\examples{
require(bigmemory)
A = matrix(1, 5, 5)
B = big.matrix(nrow=5, ncol=5, type='double', init=1)
print(B[,1])
print(A)
print(head(B, 5))
daxpy(Y=B, X=A)
print(head(B, 5))
}

