\name{bigtabulate, bigtable, bigtsummary, bigsplit, bigaggregate}
\alias{bigtabulate}
\alias{bigsplit}
\alias{bigtable}
\alias{bigtsummary}
\alias{bigaggregate}
\title{
A suite of functions for fast, efficient tabular operations (table-,
tapply-, split-, and aggregate-like) with both matrix and big.matrix
objects.
}
\description{
This package extends the \pkg{bigmemory} package, but the functions may also be
used with traditional \R \code{matrix} and \code{data.frame} objects.
The function \code{\link{bigtabulate}} is exposed, but we
expect most users will prefer the higher-level functions \code{bigtable},
\code{bigtsummary}, \code{bigsplit}, and \code{bigaggregate}.  Each of these
functions provides functionality
based on a specified conditional structure.  In other words, for every 
cell of a (possibly multidimensional) contingency table, they provide 
(or tabulate) some useful conditional behavior (or statistic(s)) of interest.
At the most basic level, this provides an extremely fast and memory-efficient
alternative to \code{\link{table}} for matrices and data frames.  However, general 
functions may be applied, and multiple functions may be applied 
simultaneously (in parallel via package \code{\link[foreach]{foreach}})
with memory and speed efficiencies.
}
\usage{
bigtabulate(x, ccols, breaks=vector("list", length=length(ccols)),
            table=TRUE, useNA="no", summary.cols=NULL,
            summary.na.rm=FALSE, splitcol=NULL, splitret="list")
bigtable(x, ccols, breaks=vector("list", length=length(ccols)),
         useNA="no")
bigtsummary(x, ccols, breaks=vector("list", length=length(ccols)),
            useNA="no", cols, na.rm=FALSE)
bigsplit(x, ccols, breaks=vector("list", length=length(ccols)),
         useNA="no", splitcol=NA, splitret="list")
bigaggregate(x, stats, usesplit=NULL,
             ccols=NA, breaks=vector("list", length=length(ccols)), 
             useNA="no", distributed=FALSE, rettype="celllist", 
             simplify=TRUE)
}
\arguments{
  \item{x}{a \code{\link[bigmemory]{big.matrix}} or a \code{\link{data.frame}} 
    or a \code{\link{matrix}}.}
  \item{ccols}{a vector of column indices or names specifying which columns 
    should be used for conditioning (e.g. for building a contingency table or 
    structure for tabulation).}
  \item{breaks}{a vector or list of \code{length(ccols)}.  If a vector, 
    \code{NA} indicates that the associated column should be treated like a 
    factor (categorical variable), while an integer value indicates that the 
    range of the associated column should be broken into a specified number of
    evenly-spaced bins (histogram-like).  If a list, \code{NA} triggers the
    factor-like handling, a single number triggers bin-like behavior, while a
    triplet (min,max,breaks) indicates that the bin-like behavior should be on
    a restricted range rather than on the range of data for that column.  See
    \code{\link[biganalytics]{binit}} for similar specification of this option.}
  \item{table}{if \code{TRUE}, a list of table counts will be returned.}
  \item{useNA}{whether to include extra '\code{NA}' levels in the table.}
  \item{summary.cols}{columns for which table summaries will be calculated.}
  \item{summary.na.rm}{if \code{TRUE}, \code{NA}s are removed from table
    summary calculations.}
  \item{splitcol}{if \code{NA}, the indices which correspond to 
    table-levels are returned.  If numeric, the corresponding column
    values will be returned in a list corresponding to table-levels.
    }
  \item{splitret}{if \code{"list"}, the \code{splitcol} value is returned
    as a list.  When \code{splitcol} is \code{NA}, \code{splitret} may
    be \code{"vector"}.}
  \item{cols}{with \code{bigtsummary}, which columns should be conditionally
    summarized?}
  \item{na.rm}{on obvious option for summaries.}
  \item{stats}{see details.}
  \item{usesplit}{if \code{bigsplit} has already produced a list of indices
    describing the conditional structure, this option may be used directly
    instead of re-specifying the structure via \code{ccols} and \code{breaks}.}
  \item{distributed}{if \code{TRUE}, the registered parallel backend to
    \code{foreach} is treated as a cluster rather than an SMP, so file-backed
    \code{big.matrix} objects will be used.}
  \item{rettype}{the only option currently implemented, \code{"celllist"}, is
    what would be expected: a tabular structured object is returned, with each
    cell containing all requested information.  An upcoming alternative will be a
    list organized by the requested operation (or statistic), with each component
    being a tabular object.}
  \item{simplify}{as with \code{\link{tapply}}, \code{FALSE} forces the return to
    be a list, while the default behavior will be an array when the result is a scalar.}
} 
\details{
This package concentrates on conditional stuctures and calculations,
much like \code{\link{table}}, \code{\link{tapply}}, \code{\link{split}},
\code{\link{aggregate}}.
The functions are juiced-up versions of the base \R functions;
they work on both regular \R matrices and data frames, but are specialized
for use with \pkg{bigmemory} and \pkg{foreach}.  They are particularly fast and
memory-efficient.  Further performance gains may be achieved
with \code{bigaggregate} via \code{\link[foreach]{foreach}} when a parallel backend
has been registered, and objects use shared memory.

When \code{x} is a \code{matrix} or a \code{data.frame}, some additional
work may be required.  For example, a character column of a \code{data.frame}
will be converted to a \code{\link{factor}} and then coerced to numeric
values (factor level numberings).  And when parallel computions are requested
by \code{bigaggregate} (because of a registered parallel backend to
\code{\link[foreach]{foreach}}), an \R object will be copied to a temporary
\code{big.matrix} in shared memory.

The conditional structure is specified via \code{ccols} and \code{breaks}
(or possible via \code{usesplit} in the case of \code{bigaggregate}).
This differs from the design of the base \R functions but is at the root
of the gains in speed and memory-efficiency.  The \code{breaks} may seem
distracting, as most users will simply condition on categorical-like columns.
However, it provides the flexibility to \dQuote{bin} \dQuote{continuous},
column(s) much like a histogram.  See \code{\link[biganalytics]{binit}} for another example
of this type of option, which can be particularly valuable with massive data sets.

A word of caution: if a \dQuote{continuous} variable is not \dQuote{binned},
it will be treated like a factor and the resulting conditional structure will
be large (perhaps immensely so).
The function uses left-closed intervals [a,b) for the "binning" behavior, 
when specified, except in the right-most bin, where the interval is entirely 
closed.

The structure of the \code{stats} option deserves special comment.  If, for
example, a summary of columns 1 and 2 (based on the specified
conditional structure) is desired, the option might be

\preformatted{
  stats=list(summary, cols=c(1,2), na.rm=TRUE)
}

In this case, groups of rows (defined by the conditional structure
of either \code{usesplit} or \code{ccols} and \code{breaks}) with only
columns 1 and 2 of \code{x} are passed to \code{\link{summary}}, and
the results are collected for each \dQuote{cell} of the conditional
structure.  \code{stats} may also be a named list of desired functions, such
as

\preformatted{
  stats=list(summary=list(summary, cols=c(1,2), na.rm=TRUE),
             myfun=list(myfun, cols=c(1,4,5)))
}           
   
where \code{myfun} is provided by the user and is applied over 3-column
submatrices of rows and columns 1, 4, and 5.  Note that different functions
can make use of different sets of columns, but the share a common
conditional structure. It is assumed that there is sufficient RAM 
to support the analysis of one (or a few) submatrices of data; however, the
use of  shared memory via \code{\link[bigmemory]{bigmemory}} can be particularly 
efficient.

Finally, \code{bigsplit} is somewhat more general than \code{split}.
The default behavior (\code{splitcol=NA})
returns a split of \code{1:nrow(x)} as a list
based on the specified conditional structure.  However, it may also
return a vector of cell (or category) numbers.  And of course it may
conduct a split of \code{x[,splitcol]}.
}
\value{
array-like object(s), each similar to what is returned by 
\code{\link{tapply}} and the associated \R functions.
}
\author{John W. Emerson and Michael J. Kane <bigmemoryauthors@gmail.com}
\seealso{\code{\link[bigmemory]{big.matrix}}, \code{\link{tapply}},
\code{\link{aggregate}}, \code{\link{table}}, \code{\link{split}}}
\examples{

data(iris)

# First, break up column 2 into 5 groups, and leave column 5 as a
# factor (which it is).  Note that iris is a data.frame, which is
# fine.  A matrix would also be fine.  A big.matrix would also be fine!
bigtable(iris, ccols=c(2, 5), breaks=list(5, NA))

iris[,2] <- round(iris[,2]) # So columns 2 and 5 will be factor-like
                            # for convenience in these examples, below:

ans1 <- bigtable(iris, c(2, 5))
ans1
# Same answer, but with nice factor labels from table(), because
# table() handles factors.  bigtable() uses the numeric factor
# levels only.
table(iris[,2], iris[,5])

# Here, our formulation is simpler than split's, and is faster and
# more memory-efficient:
ans2 <- bigsplit(iris, c(2, 5), splitcol=1)
ans2[1:3]
split(iris[,1], list(col2=factor(iris[,2]), col5=iris[,5]))[1:3]

ans3 <- bigtsummary(iris, c(2, 5), cols=1)
ans3[1:3]
by(iris[,1], list(col2=factor(iris[,2]), col5=iris[,5]),
   summary)[1:3]

# Note that the $summary result, below, is an array/list of
# non-scalar results of the summary.
ans <- bigtabulate(iris, c(2, 5), summary.cols=1, splitcol=NULL)
ans
ans$summary[1:length(ans$summary)]

}
\keyword{methods}
