\name{bigtabulate}
\alias{bigtabulate}
\title{
bigtabulate: a tapply()-like function for both matrices, data.frames, and big.matrix objects.
}
\description{
This package extends the bigmemory package for use with \code{\link{big.matrix} objects,
but also supports
use with \R \code{matrix} and \code{data.frame} objects.  The function \code{\link{bigtabulate}}
is like \code{\link{tapply}}, doing tabulations based on a specified conditional
structure.  In other words, for every cell of a (possibly multidimensional)
contingency table, it collects (tabulates) some statistic(s) of interest.  At the lowest level,
this supports an extremely fast alternative to \code{link{table}} for matrices
that also works on \code{\link{big.matrix}} objects.  But general functions
may be applied, as well, and many things may be calculated simultaneously for memory
and speed efficiencies.
}
\usage{
bigtabulate(x, ccols, breaks=vector("list", length=length(ccols)),
            stats=list("table", useNA="no"),
            distributed=FALSE, simplify=TRUE)
}
\arguments{
  \item{x}{a \code{\link{big.matrix}} or a \code{\link{data.frame}} or a \code{\link{matrix}}.}
  \item{ccols}{a vector of column indices or names specifying which columns should be used for conditioning (e.g. for building a contingency table or structure for tabulation).}
  \item{breaks}{a vector or list of \code{length(ccols)}.  If a vector, \code{NA} indicates that the associated column should be treated like a factor or categorical variable, while an integer value indicates that the range of the associated column should be broken into evenly-spaced bins (histogram-like).  If a list, \code{NA} triggers the factor-like handling, a single number triggers bin-like behavior, while a triplet (min,max,breaks) indicates that the bin-like behavior should be on a restricted range rather than on the range of data for that column.}
  \item{stats}{a list (if only one action is desired) or a named list of lists of actions;  an action is given by either a function or \code{"table"} or \code{"summary"}, the two special cases handled with particular
efficiency.  See details, below.
  \item{distributed}{if \code{TRUE}, it signals the availability of a distributed (not SMP) parallel backend for \code{\link{foreach}}; if \code{x} is a \code{matrix} or a non-shared \code{big.matrix}, a copy will be made to facilitate distribution across the network.}
  \item{simplify}{as with \code{\link{tapply}}, it forces a simpler return for atomic things (like counts, means, and so on).}
}
\details{
This function is like a juiced-up version of \code{\link{tapply}} or \code{\link{by}}
for doing calculations
within categories specified by a (potentially multivariate) contingency table.  Obvious
use of variables as factors is supported; a word of caution: if a variable is really
"continuous" but treated as categorical, this could be costly.  Optional "binning" of such
a variable could be helpful, akin to the bins used for a histogram.

The most basic operation provides an alternative to \code{\link{table}}, avoiding the
memory overhead and with improved speed.  The corresponding entry in \code{stats} would
be like \code{list("table", useNA="no")}.

The next most basic operation provides basic summary statistics for all categories
The corresponding entry in \code{stats} would be like \code{list("summary", cols=c(1,2), na.rm=TRUE)},
if summary statistics were desired for variables in columns 1 and 2 of \code{x} conditional
on values specified in columns \code{ccols}.

The most advanced operation uses any function provided by the user, with the form (for example):
\code{list(myfun, cols=c(1,2), ...)} where values of \code{x[,cols]} are summarized
conditional on values specified in columns \code{ccols=c(1,2)}.  This work is handled using
\code{\link{foreach}}, so the registration of a parallel backend can provide speed
improvements.  It is assumed that there is sufficient RAM to support the analysis
of one (or a few) blocks of data, and the use of shared memory via \code{\link{bigmemory}}
can be particularly efficient.

The function uses left-closed intervals [a,b) for the "binning" behavior, when specified,
except in the right-most bin, where the interval is entirely closed.
}
\value{
a list of array-like objects, each similar to what is returned by \code{\link{tapply}}.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{big.matrix}}, \code{\link{tapply}}}
\examples{
x <- data(iris)
x[,2] <- round(x[,2]) # So columns 2 and 5 will be factor-like.

ans <- bigtabulate( x,
                    c(2, 5),
                    stats=list(table=list("table", useNA="no"),
                               summary=list("summary", cols=c("Sepal.Length", "Petal.Width")),
                               median=list(median, cols="Petal.Length"),
                               means=list(colMeans, cols=c("Sepal.Length", "Petal.Length"), na.rm=TRUE),
                               range=list(range, cols="Petal.Length", na.rm=TRUE)),
                    distributed=FALSE, simplify=TRUE)
}
\keyword{methods}
