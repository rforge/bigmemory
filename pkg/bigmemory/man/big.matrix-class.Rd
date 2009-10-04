\name{big.matrix-class}
\docType{class}
\alias{big.matrix-class}
\alias{[<-,big.matrix,ANY,ANY-method}
\alias{[<-,big.matrix,ANY,missing-method}
\alias{[<-,big.matrix,missing,ANY-method}
\alias{[<-,big.matrix,missing,missing-method}
\alias{[,big.matrix,ANY,ANY,logical-method}
\alias{[,big.matrix,ANY,missing,logical-method}
\alias{[,big.matrix,missing,ANY,logical-method}
\alias{[,big.matrix,missing,missing,logical-method}
\alias{[,big.matrix,ANY,ANY,missing-method}
\alias{[,big.matrix,ANY,missing,missing-method}
\alias{[,big.matrix,missing,ANY,missing-method}
\alias{[,big.matrix,missing,missing,missing-method}
\alias{describe,big.matrix-method}
\alias{backingpath,big.matrix-method}
\alias{dim,big.matrix-method}
\alias{dimnames<-,big.matrix,list-method}
\alias{dimnames,big.matrix-method}
\alias{head,big.matrix-method}
\alias{ncol,big.matrix-method}
\alias{nrow,big.matrix-method}
\alias{print,big.matrix-method}
\alias{tail,big.matrix-method}
\alias{write.big.matrix,big.matrix,character-method}
\alias{read.big.matrix,character-method}
\alias{is.separated,big.matrix-method}
\alias{is.filebacked,big.matrix-method}
\alias{typeof,big.matrix-method}
\alias{mwhich,big.matrix,ANY,ANY,ANY,character-method}
\alias{mwhich,big.matrix,ANY,ANY,ANY,missing-method}
\alias{mwhich,matrix,ANY,ANY,ANY,character-method}
\alias{mwhich,matrix,ANY,ANY,ANY,missing-method}
\alias{apply,big.matrix-method}
\alias{is.big.matrix,big.matrix-method}
\alias{is.big.matrix,ANY-method}
\alias{is.sub.big.matrix,big.matrix-method}
\alias{as.big.matrix,matrix-method}
\alias{as.big.matrix,vector-method}

\title{Class ``big.matrix''.}
\description{The \code{big.matrix} class is designed for matrices with 
elements of type double, integer, short, or char.  
A \code{big.matrix} acts much like a traditional R matrix, but helps protect 
the user from many inadvertant memory-consuming pitfalls of traditional R 
matrices and data frames.  The objects are allocated to shared memory,
and if file backing is used they may exceed virtual memory in size.  Sadly,
32-bit R constraints on Windows and many MacOS machines will be a limiting
factor with file-backed matrices.}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("big.matrix", ...)}.
The functions \code{big.matrix()}
and \code{filebacked.big.matrix()} are intended for the user.
}
\section{Slots}{
	 \describe{
    \item{\code{address}:}{Object of class \code{"externalptr"} points to the memory location of the C++ data structure.}
  }
}
\section{Methods}{
  \describe{
    There are many more, but here's the idea:

    \item{[<-}{\code{signature(x = "big.matrix", i = "numeric", j = "numeric", value = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "big.matrix", i = "numeric", j = "character", value = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "big.matrix", i = "numeric", j = "missing", value = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "big.matrix", i = "numeric", j = "logical", value = "ANY")}: ... }

    \item{[}{\code{signature(x = "big.matrix", i = "numeric", j = "numeric", drop = "missing")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "numeric", j = "character", drop = "missing")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "numeric", j = "missing", drop = "missing")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "numeric", j = "logical", drop = "missing")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "numeric", j = "numeric", drop = "logical")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "numeric", j = "character", drop = "logical")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "numeric", j = "missing", drop = "logical")}: ... }
    \item{[}{\code{signature(x = "big.matrix", i = "numeric", j = "logical", drop = "logical")}: ... }

    The following may be more interesting (hopefully complete):

    \item{describe}{\code{signature(x = "big.matrix")}: provide necessary and sufficient information. }
    \item{backingpath}{\code{signature(x = "big.matrix")}: returns the backingpath of the \code{big.matrix}.}
    \item{dim}{\code{signature(x = "big.matrix")}: returns the dimension of the \code{big.matrix}. }
    \item{dimnames<-}{\code{signature(x = "big.matrix", value = "list")}: set the row and column names. }
    \item{dimnames}{\code{signature(x = "big.matrix")}: get the row and column names. }
    \item{head}{\code{signature(x = "big.matrix")}: get the first 6 (or \code{n}) rows. }
    \item{ncol}{\code{signature(x = "big.matrix")}: returns the number of columns.  }
    \item{nrow}{\code{signature(x = "big.matrix")}: returns the number of rows. }
    \item{print}{\code{signature(x = "big.matrix")}: a traditional \code{print()} is intentionally disabled, and returns \code{head(x)} unless \code{options()$bm.print.warning==FALSE}; in this case, \code{print(x[,])} is the result, which could be very big! }
    \item{tail}{\code{signature(x = "big.matrix")}: returns the last 6 (or \code{n}) rows. }
    \item{write.big.matrix}{\code{signature(bigMat = "big.matrix", fileName = "character")}: produce an ASCII file from the \code{big.matrix}. }
    \item{is.separated}{\code{signature(x = "big.matrix") }: return \code{TRUE} if the \code{big.matrix} is organized as a separated column vectors.}
    \item{typeof}{\code{signature(x = "big.matrix")}: return the type of the atomic elements of the \code{big.matrix}.}
    \item{apply}{\code{signature(x = "big.matrix")}: \code{apply()} where \code{MARGIN} may only be 1 or 2, but otherwise conforming to what you would expect from \code{apply()}.}
	 }
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{big.matrix}}}
\examples{
showClass("big.matrix")
}
\keyword{classes}
