\name{dgeev}
\Rdversion{1.1}
\alias{dgeev}
\title{BLAS dgeev functionality}
\description{
This function implements the function Y := ALPHA * X + Y where
X and Y may be either native \R matrices or \code{\link[bigmemory]{big.matrix}} objects.  
}
\usage{
dgeev(JOBVL='V', JOBVR='V', N=NULL, A, LDA=NULL, WR, WI, VL,
  LDVL=NULL, VR=NULL, LDVR=NULL, WORK=NULL, LWORK=NULL)
}
\arguments{
  \item{JOBVL}{if \code{'N'}, left eigenvectors of \code{A} are not computed; if \code{'V'}, left eigenvectors of \code{A} are computed}
  \item{JOBVR}{if \code{'N'}, right eigenvectors of \code{A} are not computed; if \code{'V'}, right eigenvectors of \code{A} are computed}
  \item{N}{the number of columns of \code{A}}
  \item{A}{the matrix to find the eigenvalues of; either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} object}
  \item{LDA}{the number of rows of \code{A}}
  \item{WR}{the real part of the eigenvalues; either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} object}
  \item{WI}{the imaginary part of the eigenvalues; either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} object}
  \item{VL}{the left eigenvectors of \code{A}; either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} object}
  \item{LDVL}{the number of columns of \code{A}}
  \item{VR}{the right eigenvectors of \code{A}; either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} objectt}
  \item{LDVR}{the number of columns of \code{A}}
  \item{WORK}{either a native \R \code{\link{matrix}} or a \code{\link[bigmemory]{big.matrix}} object}
  \item{LWORK}{the length of \code{WORK}}
}
\details{
If \code{JOBVL}, \code{JOBVR}, \code{N}, \code{LDA}, \code{LDVL}, \code{VR}, \code{LDVR}, \code{WORK},
or \code{LWORK} are not specified, appropriate default values are used.
}
\value{
The function returns \code{0} if the operation is successful.
It should be noted that the function mutates \code{Y}.
}
\references{
\url{http://www.netlib.org/lapack/double/dgeev.f}
}
\author{
Michael J. Kane
}

\seealso{
\code{\link[bigmemory]{bigmemory}}
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

