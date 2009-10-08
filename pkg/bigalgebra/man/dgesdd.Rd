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
  \item{JOBZ}{if 'A', all M columns of U and all N rows of VT are returned 
              in U and VT;  if 'S', the first min(M, N) columns of U and the 
              first min(M, N) rows of VT are returned in U and VT; if 'O', if 
              M >= N, the first N columns of U are overwritten to A 
              and all rows of VT are writted; otherwise, 
              all columns of U are returned in the array U and the first M 
              rows of VT are overwritten in A; if 'N', no columns of U or
              rows of VT are computed}
  \item{M}{the number of rows in A}
  \item{N}{the number of columns in A}
  \item{A}{either a native R matrix or a big.matrix object; it should be noted
           that A is overwritten on output;}
  \item{LDA}{the number of rows in A}
  \item{S}{the sorted singular values of A}
  \item{U}{the left singular vectors}
  \item{LDU}{the number of rows in U}
  \item{VT}{the right singular vectors}
  \item{LDVT}{the number of rows in VT}
  \item{WORK}{an internal double vector to perform the SVD}
  \item{LWORK}{the length of the WORK vector}
  \item{IWORK}{an internal integer vector to perform the SVD}
}
\details{
If JOBZ, M, N, LDA, LDU, LDVT, WORK, LWORK, or IWORK parameters are not 
specified, appropriate defaults are supplied.
}
\value{
The function returns 0 if successful, < 0 if the -i-th argument to the internal
function had an illegal value, and > 0 if the function did not converge.
}
\references{
http://www.netlib.org/lapack/double/dgesdd.f
}
\author{
Michael Kane
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
bigmemory
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

