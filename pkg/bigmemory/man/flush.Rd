\name{flush}
\alias{flush}
\title{Updating a big.matrix filebacking.}
\description{
For a filebacked \code{big.matrix} object, \code{flush()} forces
any modified information to be written to the filebacking.
}
\usage{
flush(con)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{con}{a filebacked \code{\link{big.matrix}}.}
}
\details{
This function flushes any modified data (in RAM) of a filebacked
\code{big.matrix} to its filebacking.  This may be a useful tool for
improving performance, in cases where allowing the OS to decide
on flushing creates a bottleneck near the threshold of available RAM.
}
\value{
\code{TRUE} or \code{FALSE} indicating whether or not the flush was successful.
}
\author{John W. Emerson and Michael J. Kane }
\examples{
x <- big.matrix(nrow=3, ncol=3, backingfile='flushtest.bin',
  descriptorfile='flushtest.desc', type='integer')
x[1,1] <- 0
flush(x)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
