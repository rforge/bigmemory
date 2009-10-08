\name{dscal}
\Rdversion{1.1}
\alias{dscal}
\title{BLAS dscal functionality}
\description{
This function implements the function Y := ALPHA * Y where
Y may be either native R matrices or big.matrix objects.  
}
\usage{
dscal(N=NULL, ALPHA, Y, INCY=1)
}
\arguments{
  \item{N}{the number of elements in the matrix}
  \item{ALPHA}{the value to scale the matrix by}
  \item{Y}{either a native R matrix or a big.matrix object}
  \item{INCY}{spacing between elements to be copied}
}
\details{
If N and INCY are not specified, the default is scaling all 
elements of Y
}
\value{
The function returns 0 if the operation is successful.
It should be noted that the function mutates Y.
}
\references{
http://www.netlib.org/blas/dscal.f
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
B = big.matrix(nrow=5, ncol=5, type='double')
print(A)
dcopy(Y=B, X=A)
print(head(B, 5))
a = 2
dscal(ALPHA=a, Y=A)
print(A)
dscal(ALPHA=a, Y=B)
print(head(B, 5))
}

