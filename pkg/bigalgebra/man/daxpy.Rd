\name{daxpy}
\Rdversion{1.1}
\alias{daxpy}
\title{BLAS daxpy functionality}
\description{
This function implements the function Y := ALPHA * X + Y where
X and Y may be either native R matrices or big.matrix objects.  
}
\usage{
daxpy(N=NULL, ALPHA=1, X, INCX=1, Y, INCY=1)
}
\arguments{
  \item{N}{the number of elements in the matrix}
  \item{ALPHA}{the value to scale the matrix X by}
  \item{X}{either a native R matrix or a big.matrix object}
  \item{INCX}{spacing between elements of X to be copied}
  \item{Y}{either a native R matrix or a big.matrix object}
  \item{INCY}{spacing between elements of Y to be copied}
}
\details{
If N, INCX, INCY are not specified, the default is addition of all
elements of X to Y
}
\value{
The function returns 0 if the operation is successful.
It should be noted that the function mutates Y.
}
\references{
http://www.netlib.org/blas/daxpy.f
}
\author{
Michael Kane
}

\seealso{
bigmemory
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

