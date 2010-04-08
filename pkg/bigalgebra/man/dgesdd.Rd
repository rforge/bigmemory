\name{dgesdd}
\Rdversion{1.1}
\alias{dgesdd}
\title{LAPACK dgesdd functionality}
\description{
This function implements the function singular value decomposition 
A := U \%*\% SIGMA \%*\% transpose(V), where U is an M-by-M orthogonal
matrix, SIGMA is an M-by-N matrix which is zero except for its min(M,N)
diagonal elemtns, and V is an N-by-N orthogonal matrix.
}
\usage{
dgesdd( JOBZ='A', M=NULL, N=NULL, A, LDA=NULL, S, U, LDU=NULL,
  VT, LDVT=NULL, WORK=NULL, LWORK=NULL, IWORK=NULL)
}
\arguments{
  \item{JOBZ}{if \code{'A'}, all \code{M} columns of \code{U} and all \code{N} rows of \code{VT} are returned 
              in \code{U} and \code{VT};  if \code{'S'}, the first \code{min(M, N)} columns of \code{U} and the 
              first min(M, N) rows of \code{VT} are returned in \code{U} and \code{VT}; if \code{'O'}, if 
              \code{M >= N}, the first \code{N} columns of \code{U} are overwritten to \code{A} 
              and all rows of \code{VT} are writted; otherwise, 
              all columns of \code{U} are returned in the array \code{U} and the first \code{M} 
              rows of \code{VT} are overwritten in \code{A}; if \code{'N'}, no columns of \code{U} or
              rows of \code{VT} are computed}
  \item{M}{the number of rows in \code{A}}
  \item{N}{the number of columns in \code{A}}
  \item{A}{either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} object; it should be noted that \code{A} is overwritten on output;}
  \item{LDA}{the number of rows in \code{A}}
  \item{S}{the sorted singular values of \code{A}}
  \item{U}{the left singular vectors}
  \item{LDU}{the number of rows in \code{U}}
  \item{VT}{the right singular vectors}
  \item{LDVT}{the number of rows in \code{VT}}
  \item{WORK}{an internal double vector to perform the SVD}
  \item{LWORK}{the length of the \code{WORK} vector}
  \item{IWORK}{an internal integer vector to perform the SVD}
}
\details{
If \code{JOBZ}, \code{M}, \code{N}, \code{LDA}, \code{LDU}, \code{LDVT}, \code{WORK},
\code{LWORK}, or \code{IWORK} parameters are not 
specified, appropriate defaults are supplied.
}
\value{
The function returns 0 if successful, < 0 if the -i-th argument to the internal
function had an illegal value, and > 0 if the function did not converge.
}
\references{
\url{http://www.netlib.org/lapack/double/dgesdd.f}
}
\author{
Michael J. Kane
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\url{http://www.netlib.org/blas/daxpy.f}
}
\examples{
require(bigmemory)
m=9
n=6
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
A = hilbert(m)[,1:n]
print(A)
print(svd(A))
AA = as.big.matrix(A, type='double')
U = matrix(0, nrow=m, ncol=m)
S = as.matrix(rep(0.0, m))
VT = matrix(0, nrow=n, ncol=n)
dgesdd(A=A, S=S, U=U, VT=VT)
print(A)
print(U)
print(S)
print(VT)
}

