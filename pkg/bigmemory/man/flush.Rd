\name{flush}
\alias{flush}
\title{Updating a \code{big.matrix} filebacking.}
\description{
For filebacked \code{big.matrix} object, flush written information to the
filebacking.
}
\usage{
flush(con)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con}{a file-backed \code{\link{big.matrix}}.}
}
\details{
This function flushes data that has been written to a filebacked 
\code{big.matrix} to it filebacking.
}
\value{
\code{TRUE} or \code{FALSE} indicating whether or not the flush was successful.
}
\author{John W. Emerson and Michael J. Kane }
\examples{
x = big.matrix(nrow=3, ncol=3, backingfile='flushtest.bin',
  descriptorfile='flushtest.desc', type='integer')
x[1,1] = 0
flush(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
