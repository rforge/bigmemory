\name{bigkmeans}
\alias{bigkmeans}
\title{The Bigmemory Project's memory-efficient k-means cluster analysis}
\description{
k-means cluster analysis without the memory overhead, and possibly
in parallel using shared memory.
}
\usage{
bigkmeans(x, centers, iter.max = 10, nstart = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a \code{\link[bigmemory]{big.matrix}} object.}
  \item{centers}{a scalar denoting the number of clusters, or for k clusters, a k by \code{ncol(x)} matrix.}
  \item{iter.max}{the maximum number of iterations.}
  \item{nstart}{number of random starts, to be done in parallel if there is a registered backend (see below).}
}
\details{
The real benefit is the lack of memory overhead compared to the standard
\code{\link{kmeans}} function.  Part of the overhead from \code{kmeans()} stems
from the way it looks for unique starting centers, and could be improved
upon.  The \code{bigkmeans()} function works on either regular \R
\code{matrix} objects, or on \code{big.matrix} objects.  In either case, it
requires no extra
memory (beyond the data, other than recording the cluster memberships), 
whereas \code{kmeans()} makes at least two extra copies of the data.  And
\code{kmeans()} is even worse if multiple starts (\code{nstart>1}) are used.

If \code{nstart>1} and you are using \code{bigkmeans()} in parallel,
a vector of cluster memberships 
will need to be stored for each worker, which could be 
memory-intensive for large data.  This isn't a problem if you use are running
the multiple starts sequentially.

Unless you have a really big data set (where a single run of \code{\link{kmeans}}
not only burns memory but takes more than a few seconds), use of parallel
computing for multiple random starts is unlikely to be much 
faster than running iteratively.
}
\note{
A comment should be made about the excellent package \pkg{foreach}.  By
default, it behaves much like a \code{for} loop here over the \code{nstart}
random starting points.  Even so, there are efficiencies, doing a comparison
of each result to the previous best, rather than saving everything and doing
a final comparison.

When a parallel backend has been registered (see \pkg{doSNOW}, \pkg{doMC},
and \pkg{doMPI}, for example), \code{bigkmeans()} automatically distributes
the \code{nstart} random starting points across the available workers.  This
is done in shared memory on an SMP, and is distributed on a cluster, *IF*
the \code{big.matrix} is filebacked.  If used on a cluster with an in-RAM
\code{big.matrix}, it will fail horribly.  We're considering an extra option
as an alternative to the current behavior.
}
\value{
An object of class \code{kmeans}, just as produced by \code{\link{kmeans}}.
}
\author{John W. Emerson and Michael J. Kane }
\seealso{\code{\link[bigmemory]{big.matrix}}, \code{\link[foreach]{foreach}}}
\examples{
  # Simple example (with one processor):

  library(bigmemory)

  x <- big.matrix(100000, 3, init=0, type="double")
  x[seq(1,100000,by=2),] <- rnorm(150000)
  x[seq(2,100000,by=2),] <- rnorm(150000, 5, 1)
  head(x)
  ans <- bigkmeans(x, 1)              # One cluster isn't always allowed
                                      # but is convenient.
  ans$centers
  ans$withinss
  ans$size
  apply(x, 2, mean)
  ans <- bigkmeans(x, 2, nstart=5)    # Sequential multiple starts.
  class(ans)
  names(ans)
  ans$centers
  ans$withinss
  ans$size

  # To use a parallel backend, try something like the following,
  # assuming you have at least 3 cores available on this machine.
  # Each processor does incur memory overhead for the storage of
  # cluster memberships.
  \dontrun{
    library(doSNOW)
    cl <- makeCluster(3, type="SOCK")
    registerDoSNOW()
    ans <- bigkmeans(x, 2, nstart=5)
  }

  # Both the following are run iteratively, but with less memory overhead using
  # bigkmeans().  Note that the gc() comparisons aren't completely fair, because
  # the big.matrix objects aren't reflected in the gc() summary.  But the
  # savings is there.
  gc(reset=TRUE)
  time.new <- system.time(print(bigkmeans(x, 2, nstart=5)$centers))
  gc()
  y <- x[,]
  rm(x)
  gc(reset=TRUE)
  time.old <- system.time(print(kmeans(y, 2, nstart=5)$centers))
  gc()
  # The new kmeans() centers should match the old kmeans() centers, without
  # the memory overhead amd running more quickly.
  time.new
  time.old
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{methods}
