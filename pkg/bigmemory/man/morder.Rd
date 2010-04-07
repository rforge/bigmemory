\name{morder, mreorder}
\alias{morder}
\alias{mreorder}
\title{Ordering functions for ``big.matrix'' and ``matrix'' objects}
\description{
The \code{morder} function returns the ordering of a column of a 
\code{big.matrix} or \code{matrix} object.  The reorder function
reorders the rows of a \code{big.matrix} or \code{matrix} based on 
an order vector or an ordering on a set of columns.
}
\usage{
morder(x, cols, na.last=TRUE, decreasing = FALSE)
mreorder(x, order=NULL, cols=NULL, allow.duplicates=FALSE, ...)
}
\arguments{
\item{x}{A \code{big.matrix} or \code{matrix} object with numeric values.}
\item{cols}{The columns of \code{x} to get the ordering for or reorder on}
\item{na.last}{for controlling the treatment of \code{NA}s. If 
  \code{TRUE}, missing values in the data are put last; if \code{FALSE}, 
  they are put first; if \code{NA}, they are removed.}
\item{decreasing}{logical. Should the sort order be increasing or decreasing?}
\item{order}{A vector specifying the reordering of rows.}
\item{allow.duplicates}{If \code{TRUE}, allows a row to be dupicated in
the resulting \code{big.matrix} or \code{matrix}.}
\item{...}{Parameters to pass to \code{morder} when \code{cols} is \code{TRUE}.
}
}
\details{ The \code{morder} function behaves similar to \code{order}, returning
a sorted ordering of a set of vectors.  However, \code{morder} takes
a \code{big.matrix} or R \code{matrix} (with numeric type) an a set of 
columns (\code{cols}) to perform the ordering on.  \code{morder} does
not incur the same memory overhead required by \code{order} and 
it runs more quickly.

The \code{mreorder} function changes the row ordering of a \code{big.matrix}
or \code{matrix} based on a vector \code{order} or an ordering based
on a set of columns specifed by \code{cols}.  It should be noted that
this function has side-effects, that is \code{x} is changed when this
function is called.
}
\value{
\code{morder} returns an ordering vector.

\code{mreorder} returns nothing but does change the matrix \code{x} which
is passed as a parameter.
}
\author{Michael J. Kane and John W. Emerson}
\seealso{\code{\link{order}}
}
\examples{
m = matrix( as.double(as.matrix(iris)), nrow=nrow(iris) )
morder(m, 1)
order(m[,1])

m[order(m[,1]), 2]
mreorder(m, cols=1)
m[,2]


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
