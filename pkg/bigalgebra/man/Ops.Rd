\name{balgebra-methods}
\docType{class}
\alias{\%*\%,big.matrix,big.matrix-method}
\alias{\%*\%,matrix,big.matrix-method}
\alias{\%*\%,big.matrix,matrix-method}
\title{Class \code{"big.matrix"} arithmetic methods}
\description{Arithmetic operations for big.matrices}
\section{Methods}{
  \describe{
    \item{\%*\%}{\code{signature{x="big.matrix", y="big.matrix"}}: ...}
    \item{\%*\%}{\code{signature{x="matrix", y="big.matrix"}}: ...}
    \item{\%*\%}{\code{signature{x="big.matrix", y="matrix"}}: ...}
  }
}
\section{Notes}{
Miscellaneous arithmetic methods for matrices and big.matrices. See
also \code{options("bigalgebra.mixed_arithmetic_returns_R_matrix")}.
}
\author{
B. W. Lewis <blewis@illposed.net>
}
