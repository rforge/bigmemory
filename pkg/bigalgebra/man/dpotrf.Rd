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
  \item{UPLO}{if \code{'U'}, the upper triangle of \code{A} is stored; if \code{'L'}, the lower
              triangle of \code{A} is stored}
  \item{N}{the number of columns of \code{A}}
  \item{A}{either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} object}
  \item{LDA}{the number of rows of \code{A}}
}
\details{
If \code{UPLO} is not specified, \code{'U'} is assumed.  If \code{N} or \code{LDA} are not specified,
appropriate defaults are used.
}
\value{
The function returns 0 if the operation is successful.
It should be noted that the function mutates \code{A}.
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
A = matrix(c(5,1,1,3),2,2)
B = as.big.matrix(A)
print(chol(A))
dpotrf(A=A)
print(A)
dpotrf(A=B)
print(head(B, 2))
}

