\name{dgeev}
\Rdversion{1.1}
\alias{dgeev}
\title{BLAS dgeev functionality}
\description{
This function implements the function Y := ALPHA * X + Y where
X and Y may be either native R matrices or big.matrix objects.  
}
\usage{
dgeev(JOBVL='V', JOBVR='V', N=NULL, A, LDA=NULL, WR, WI, VL,
  LDVL=NULL, VR=NULL, LDVR=NULL, WORK=NULL, LWORK=NULL)
}
\arguments{
  \item{JOBVL}{if 'N', left eigenvectors of A are not computed; if 'V',
               left eigenvectors of A are computed}
  \item{JOBVR}{if 'N', right eigenvectors of A are not computed; if 'V',
               right eigenvectors of A are computed}
  \item{N}{the number of columns of A}
  \item{A}{the matrix to find the eigen values of; either a native R matrix or
           a big.matrix object}
  \item{LDA}{the number of rows of A}
  \item{WR}{the real part of the eigen values; either a native R matrix or a
           big.matrix object}
  \item{WI}{the imaginary part of the eigen values; either a native R matrix 
           or a big.matrix object}
  \item{VL}{the left eigen vectors of A; either a native R matrix 
           or a big.matrix object}
  \item{LDVL}{the number of columns of A}
  \item{VR}{the right eigen vectors of A; either a native R matrix 
           or a big.matrix object}
  \item{LDVR}{the number of columns of A}
  \item{WORK}{either a native R matrix or a big.matrix object}
  \item{LWORK}{the length of WORK}
}
\details{
If JOBVL, JOBVR, N, LDA, LDVL, VR, LDVR, WORK, or LWORK are not specified,
appropriate default values are provided
}
\value{
The function returns 0 if the operation is successful.
It should be noted that the function mutates Y.
}
\references{
http://www.netlib.org/lapack/double/dgeev.f
}
\author{
Michael Kane
}

\seealso{
bigmemory
}
\examples{
require(bigmemory)
A = cbind(c(1.0,-1.0),c(-1.0,1.0))
B = as.big.matrix(A)
print(eigen(A))
VR = cbind(c(1.0,-1.0),c(-1.0,1.0))
VL = cbind(c(1.0,-1.0),c(-1.0,1.0))
WR = as.matrix(rep(0.0, 2))
WI = as.matrix(rep(0.0, 2))
dgeev(A=A, WR=WR, WI=WI, VR=VR, VL=VL)
print(A)
print(WR)
print(WI)
print(VR)
print(VL)
dgeev(A=B, WR=WR, WI=WI, VR=VR, VL=VL)
print(head(B, 2))
print(WR)
print(WI)
print(VR)
print(VL)
}

