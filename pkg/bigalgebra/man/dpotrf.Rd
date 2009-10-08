\name{dpotrf}
\Rdversion{1.1}
\alias{dpotrf}
\title{BLAS dpotrf functionality}
\description{
This function implements the Cholesky factorization of a real, symmetric,
positive definite matrix A. 
}
\usage{
dpotrf(UPLO='U', N=NULL, A, LDA=NULL)
}
\arguments{
  \item{UPLO}{if 'U', the upper triangle of A is stored; if 'L', the lower
              triangle of A is stored}
  \item{N}{the number of columns of A}
  \item{A}{either a native R matrix or a big.matrix object}
  \item{LDA}{the number of rows of A}
}
\details{
If UPLO is not specified 'U' is assumed.  If N or LDA are not specified,
appropriate defaults are used.
}
\value{
The function returns 0 if the operation is successful.
It should be noted that the function mutates A.
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
A = matrix(c(5,1,1,3),2,2)
B = as.big.matrix(A)
print(chol(A))
dpotrf(A=A)
print(A)
dpotrf(A=B)
print(head(B, 2))
}

