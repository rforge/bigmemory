\name{dcopy}
\Rdversion{1.1}
\alias{dcopy}
\title{BLAS dcopy functionality}
\description{
This function implements the function Y := X where
Y and X may be either native R matrices or big.matrix objects.  
}
\usage{
dcopy(N=NULL, X, INCX=1, Y, INCY=1)
}
\arguments{
  \item{N}{the number of elements in the matrix}
  \item{X}{eith a native R matrix or a big.matrix object}
  \item{INCX}{spacing between elements to copy}
  \item{Y}{eith a native R matrix or a big.matrix object}
  \item{INCY}{spacing between elements to be copied}
}
\details{
If N, INCX, and INCY are not specified, the default is a copy of all 
elements from X to Y
}
\value{
The function returns 0 if the operation is successful.  It should be
noted that the function mutates Y.
}
\references{
http://www.netlib.org/blas/dcopy.f
}
\author{
Michael Kane
}

\seealso{
bigmemory
}
\examples{
require(bigmemory)
A = matrix(rnorm(25), 5, 5)
B = matrix(0, 5, 5)
C = big.matrix(nrow=5, ncol=5, type='double')
print(A)
print(B)
print(head(C, 5))
# Copy from A to C
dcopy(Y=C, X=A)
print(head(C,5))
# Now copy from C to B
dcopy(Y=B, X=C)
print(B)
}

