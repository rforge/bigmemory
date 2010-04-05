\name{bigtabulate}
\alias{bigtabulate}
\alias{bigsplit}
\alias{bigtable}
\alias{bigtsummary}
\alias{bigaggregate}
\title{
bigtabulate: a tapply()-like function for both matrices, data.frames, and 
big.matrix objects.
}
\description{
This package extends the bigmemory package for use with 
\code{\link[bigmemory]{big.matrix}} objects, but also supports
use with \R \code{matrix} and \code{data.frame} objects.  The function 
\code{\link{bigtabulate}} is like \code{\link{tapply}}, doing tabulations 
based on a specified conditional structure.  In other words, for every 
cell of a (possibly multidimensional) contingency table, it collects 
(tabulates) some statistic(s) of interest.  At the lowest level,
this supports an extremely fast alternative to \code{\link{table}} for matrices
that also works on \code{\link[bigmemory]{big.matrix}} objects.  But general 
functions may be applied, as well, and many things may be calculated 
simultaneously for memory and speed efficiencies.
}
\usage{
bigtabulate(x, ccols, breaks=vector("list", length=length(ccols)),
            table=TRUE, useNA="no", summary.cols=NULL,
            summary.na.rm=FALSE, splitcol=NULL, splitret="list")
}
\arguments{
  \item{x}{a \code{\link[bigmemory]{big.matrix}} or a \code{\link{data.frame}} 
    or a \code{\link{matrix}}.}
  \item{ccols}{a vector of column indices or names specifying which columns 
    should be used for conditioning (e.g. for building a contingency table or 
    structure for tabulation).}
  \item{breaks}{a vector or list of \code{length(ccols)}.  If a vector, 
    \code{NA} indicates that the associated column should be treated like a 
    factor or categorical variable, while an integer value indicates that the 
    range of the associated column should be broken into evenly-spaced bins 
    (histogram-like).  If a list, \code{NA} triggers the factor-like handling, 
    a single number triggers bin-like behavior, while a triplet (min,max,breaks)    indicates that the bin-like behavior should be on a restricted range 
    rather than on the range of data for that column.}
  \item{table}{if \code{TRUE} a list of table counts will be return.}
  \item{useNA}{whether to include extra '\code{NA}' levels in the table.}
  \item{summary.cols}{columns where table summaries will be calculated.}
  \item{summary.na.rm}{if \code{TRUE}, \code{NA}'s are removed from table
    summary calculations.}
  \item{splitcol}{if \code{NA}, the indices which correspond to 
    table-levels are returned.  If numeric, the corresponding column
    values will be returned in a list corresponding to table-levels.
    }
  \item{splitret}{if \code{"list"}, the \code{splitcol} value is returned
    as a list.  When \code{splitcol} is \code{NA}, \code{splitret} may
    be \code{"vector"}.}
}
\details{
This function is like a juiced-up version of \code{\link{tapply}} or 
\code{\link{by}} for doing calculations within categories specified by a 
(potentially multivariate) contingency table.  Obvious use of variables as 
factors is supported; a word of caution: if a variable is really
"continuous" but treated as categorical, this could be costly.  Optional 
"binning" of such a variable could be helpful, akin to the bins used for a 
histogram.

The most basic operation provides an alternative to \code{\link{table}}, 
avoiding the memory overhead and with improved speed.  The corresponding entry 
in \code{stats} would be like \code{list("table", useNA="no")}.

The next most basic operation provides basic summary statistics for all 
categories The corresponding entry in \code{stats} would be like 
\code{list("summary", cols=c(1,2), na.rm=TRUE)}, if summary statistics were 
desired for variables in columns 1 and 2 of \code{x} conditional on values 
specified in columns \code{ccols}.

The most advanced operation uses any function provided by the user, with the 
form (for example): \code{list(myfun, cols=c(1,2), ...)} where values of 
\code{x[,cols]} are summarized conditional on values specified in columns 
\code{ccols=c(1,2)}.  This work is handled using 
\code{\link[foreach]{foreach}}, so the registration of a parallel backend 
can provide speed improvements.  It is assumed that there is sufficient RAM 
to support the analysis of one (or a few) blocks of data, and the use of 
shared memory via \code{\link[bigmemory]{bigmemory}} can be particularly 
efficient.

The function uses left-closed intervals [a,b) for the "binning" behavior, 
when specified, except in the right-most bin, where the interval is entirely 
closed.
}
\value{
a list of array-like objects, each similar to what is returned by 
\code{\link{tapply}}.
}
\author{John W. Emerson and Michael J. Kane <bigmemoryauthors@gmail.com}
\seealso{\code{\link[bigmemory]{big.matrix}}, \code{\link{tapply}}}
\examples{
data(iris)
iris[,2] <- round(iris[,2]) # So columns 2 and 5 will be factor-like.

ans1 <- bigtable(iris, c(2, 5))
ans1
table(iris[,2], iris[,5])

#ans2 <- bigsplit(iris, c(2, 5), splitcol=1)
#ans2
#by(iris, list(factor(iris[,2]), factor(iris[,5])), function(x) return(x[,1]))

ans3 <- bigtsummary(iris, c(2, 5), cols=1)
ans3[1:9]
by(iris[,1], list(factor(iris[,2]), factor(iris[,5])), summary)

}
\keyword{methods}
