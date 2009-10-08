\name{dgemm}
\Rdversion{1.1}
\alias{dgemm}
\title{BLAS dgemm functionality}
\description{
This function implements the function C = ALPHA*op(A) * op(B) + BETA*C where
A, B, and C may be either native R matrices or big.matrix objects.  
Note: parameter descriptions were taken from http://www.netlib.org/blas/dgemm.f
}
\usage{
dgemm(TRANSA='n', TRANSB='n', M=NULL, N=NULL, K=NULL,
  ALPHA=1, A, LDA=NULL, B, LDB=NULL, BETA=0, C, LDC=NULL)
}
\arguments{
  \item{TRANSA}{ specifies the form of op( A ) to be used in the matrix 
    multiplication as follows: TRANSA = 'N' or 'n',  op( A ) = A;
    TRANSA = 'T' or 't',  op( A ) = A'; TRANSA = 'C' or 'c',  op( A ) = A'
  }
  \item{TRANSB}{see TRANSA}
  \item{M}{specifies  the number  of rows  of the  matrix
    op( A )  and of the  matrix  C;  M  must  be at least  zero.}
  \item{N}{
    specifies the number  of columns of the matrix
    op( B ) and the number of columns of the matrix C. N must be
    at least zero.}
  \item{K}{
    specifies  the number of columns of the matrix
    op( A ) and the number of rows of the matrix op( B ); K must
    be at least  zero}
  \item{ALPHA}{specifies the scalar alpha}
  \item{A}{either a native R matrix or a big.matrix object}
  \item{LDA}{on entry, LDA specifies the first dimension of A as declared
    in the calling (sub) program; when  TRANSA = 'N' or 'n' then
    LDA must be at least  max( 1, m ), otherwise  LDA must be at
    least  max( 1, k )}
  \item{B}{either a native R matrix or a big.matrix object}
  \item{LDB}{see LDB}
  \item{BETA}{specifies the scalar beta}
  \item{C}{either a native R matrix or a big.matrix object}
  \item{LDC}{see LDA}
}
\details{
If LD*, K, M, or N parameters are not specified, the default is the 
appropriate number of rows or columns of the matrix based on the TRANS*
arguments.
}
\value{
The function returns 0 if the operation is successful.
It should be noted that the function mutates C.
}
\references{
http://www.netlib.org/blas/dgemm.f
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
A = matrix(rnorm(100), 10)
B = matrix(rnorm(100), 10)
C = matrix(0, 10, 10)
print(A \%*\% B)
dgemm(C=C, A=A, B=B)
print(C)
A = as.big.matrix(A, type='double')
C = as.big.matrix(C, type='double')
dgemm(C=C, A=A, B=B)
print(head(C, 10))
}

