\name{dgemm}
\Rdversion{1.1}
\alias{dgemm}
\title{BLAS dgemm functionality}
\description{
This function implements the function C = ALPHA*op(A) * op(B) + BETA*C where
A, B, and C may be either native R matrices or \code{\link[bigmemory]{big.matrix}} objects.  
The parameter descriptions, below, were taken from \url{http://www.netlib.org/blas/dgemm.f}.
}
\usage{
dgemm(TRANSA='n', TRANSB='n', M=NULL, N=NULL, K=NULL,
  ALPHA=1, A, LDA=NULL, B, LDB=NULL, BETA=0, C, LDC=NULL, COFF = 0)
}
\arguments{
  \item{TRANSA}{ specifies the form of op( A ) to be used in the matrix 
    multiplication as follows: TRANSA = 'N' or 'n',  op( A ) = A;
    TRANSA = 'T' or 't',  op( A ) = A'; TRANSA = 'C' or 'c',  op( A ) = A'
  }
  \item{TRANSB}{see \code{TRANSA}}
  \item{M}{specifies  the number  of rows  of the  matrix \code{A}  and of the  matrix \code{C};  \code{M}  must  be at least  zero.}
  \item{N}{specifies the number  of columns of the matrix \code{B} and the number of columns of the matrix \code{C}; \code{N} must be at least zero.}
  \item{K}{specifies  the number of columns of the matrix \code{A} and the number of rows of the matrix \code{B}; \code{K} must be at least  zero}
  \item{ALPHA}{specifies the scalar alpha}
  \item{A}{either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} object}
  \item{LDA}{on entry, \code{LDA} specifies the first dimension of \code{A} as declared in the calling (sub) program; when  \code{TRANSA} equals \code{'N'} or \code{'n'} then \code{LDA} must be at least  \code{max(1, m)}, otherwise \code{LDA} must be at least  \code{max(1, k)}}
  \item{B}{either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} object}
  \item{LDB}{see \code{LDB}}
  \item{BETA}{specifies the scalar beta}
  \item{C}{either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} object}
  \item{LDC}{see \code{LDA}}
  \item{COFF}{not sure}
}
\details{
If \code{LD*}, \code{K}, \code{M}, or \code{N} parameters are not specified, the default is the 
appropriate number of rows or columns of the matrix based on the \code{TRANS*}
arguments.
}
\value{
The function returns \code{0} if the operation is successful.
It should be noted that the function mutates \code{C}.
}
\references{
\url{http://www.netlib.org/blas/dgemm.f}
}
\author{
Michael J. Kane
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[bigmemory]{bigmemory}}
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

