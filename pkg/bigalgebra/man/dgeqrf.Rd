\name{dgeqrf}
\Rdversion{1.1}
\alias{dgeqrf}
\title{LAPACK dgeqrf functionality}
\description{
This function computes the QR factorization of A.
}
\usage{
dgeqrf(M=NULL, N=NULL, A, LDA=NULL, TAU=NULL, WORK=NULL, LWORK=NULL)
}
\arguments{
  \item{M}{the number of rows of A}
  \item{N}{the number of columns of A}
  \item{A}{either a native R matrix or a big.matrix object}
  \item{LDA}{spacing between elements of X to be copied}
  \item{TAU}{either a native R matrix or a big.matrix object}
  \item{WORK}{either a native R matrix or a big.matrix object}
  \item{LWORK}{the number of columns of A}
}
\details{
If N, INCX, INCY are not specified, the default is addition of all
elements of X to Y.
}
\value{
The function returns 0 if the operation is successful.
It should be noted that the function mutates Y.
}
\references{
http://www.netlib.org/lapack/double/dgeqrf.f
}
\author{
Michael Kane
}

\seealso{
bigmemory
}
\examples{
require(bigmemory)
hilbert = function(n) { i = 1:n; 1 / outer(i - 1, i, "+") }
A = hilbert(9)
B = as.big.matrix(A)
print(A)
print(qr(A))
dgeqrf(A=A)
print(A)
dgeqrf(A=B)
print(head(B, 9))
}

