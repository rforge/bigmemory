\name{big.matrix, filebacked.big.matrix, etc...}
\alias{big.matrix}
\alias{filebacked.big.matrix}
\alias{as.big.matrix}
\alias{is.big.matrix}
\alias{is.separated}
\alias{is.filebacked}
\alias{is.shared}
\alias{[.big.matrix}
\title{The core ``big.matrix'' operations.}
\description{
Create a \code{big.matrix} (or check to see if an object is a \code{big.matrix},
or create a \code{big.matrix} from a \code{\link{matrix}}, and so on).  The \code{big.matrix}
may be file-backed.
}
\usage{
big.matrix(nrow, ncol, type = "integer", init = NULL,
  dimnames = NULL, separated = FALSE, backingfile = NULL,
  backingpath = NULL, descriptorfile = NULL, shared = TRUE)
filebacked.big.matrix(nrow, ncol, type = "integer", init = NULL, 
  dimnames = NULL, separated = FALSE, backingfile = NULL,
  backingpath = NULL, descriptorfile = NULL)
as.big.matrix(x, type = NULL, separated = FALSE,
  backingfile = NULL, backingpath = NULL, descriptorfile = NULL)
is.big.matrix(x)
is.separated(x)
is.filebacked(x)
is.shared(x)
}
\arguments{
  \item{x}{a \code{matrix} or \code{vector} for \code{as.big.matrix}; if a vector, a one-column\cr \code{big.matrix} is created by \code{as.big.matrix}.}
  \item{nrow}{number of rows.  For the \code{is.*} functions, \code{x} is likely a \code{big.matrix}.}
  \item{ncol}{number of columns.}
  \item{type}{the type of the atomic element (\code{"integer"} by default).}
  \item{init}{a scalar value for initializing the matrix (NULL by default to avoid unnecessary time spent doing the initializing).}
  \item{dimnames}{a list of the row and column names.}
  \item{separated}{use separated column organization of the data; see details.}
  \item{backingfile}{the root name for the file(s) for the cache of \code{x}.}
  \item{backingpath}{the path to the directory containing the file backing cache.}
  \item{descriptorfile}{the name of the file to hold the filebacked description, for subsequent use with \code{\link{attach.big.matrix}}; if \code{NULL}, the \code{backingfile} is used as the root.  The descriptor file is placed in the same directory as the backing files.}
  \item{shared}{\code{TRUE} by default, and always \code{TRUE} if the \code{big.matrix} is filebacked.  For a non-filebacked \code{big.matrix}, \code{shared=FALSE} uses non-shared memory, which can be more stable for large (say, >50\% of RAM) objects.  Shared memory allocation can sometimes fail in such cases due to exhausted shared-memory resources in the system.}
}
\details{
A \code{big.matrix} consists of an object in \R that does nothing more than point to
the data structure implemented in \acronym{C++}.  The object acts
much like a traditional \R matrix, but helps protect the user from many inadvertant
memory-consuming pitfalls of traditional \R matrices and data frames.

There are two \code{big.matrix} types which manage
data in different ways.  A standard, shared \code{big.matrix} is constrained
to available \acronym{RAM}, and may be shared across
separate Rprocesses.  A file-backed \code{big.matrix} may
exceed available \acronym{RAM} by using hard drive space, and may also be
shared across processes.  The atomic types of these matrices may be
\code{double}, \code{integer}, \code{short}, or \code{char}
(8, 4, 2, and 1 bytes, respectively).

If \code{x} is a \code{big.matrix}, then \code{x[1:5,]} is returned as an R
\code{matrix} containing the first five rows of \code{x}.  If \code{x} is of type
\code{double}, then the result will be \code{numeric}; otherwise, the result will
be an \code{integer} R matrix.  The expression \code{x} alone
will display information about the R object (e.g. the external pointer) rather
than evaluating the matrix itself (the user should try \code{x[,]} with extreme caution,
recognizing that a huge R \code{matrix} will be created).

If \code{x} has a huge number of rows and/or columns, then the use of \code{rownames}
and/or \code{colnames}
will be extremely memory-intensive and should be avoided.  If \code{x} has a huge
number of columns and \code{separated=TRUE} is used (this isn't typically recommended),
the user might want to store the transpose as there is 
overhead of a pointer for each column in the matrix.
If \code{separated} is \code{TRUE}, then the memory is allocated into separate
vectors for each column.  If \code{separated} is \code{FALSE}, the matrix is
stored in traditional column-major format.  
The function \code{is.separated()} returns
the separation type of the \code{big.matrix}.

When a \code{big.matrix}, \code{x}, is passed as an argument
to a function, it is essentially providing call-by-reference rather than
call-by-value behavior.  If the function modifies any of the values of \code{x},
the changes are not limited in scope to a local copy within the function.
This introduces the possibility of side-effects, in contrast to standard
\R behavior.

A file-backed \code{big.matrix} may exceed available \acronym{RAM} in size by using a file
cache (or possibly multiple file caches, if \code{separated=TRUE}).
This can incur a substantial performance penalty for such large matrices, but could
be useful nonetheless.  A side-effect of creating a filebacked object is
not only the filebacking(s), but a descriptor file (in the same directory) that can
be used for subsequent attachments (see \code{\link{attach.big.matrix}}).

Note that we do not allow setting or changing the \code{dimnames} attributes
by default; such changes would not be reflected in the descriptor objects or
in shared memory.  To override this, set\cr
\code{options(bigmemory.allow.dimnames=TRUE)}.
}
\value{
A \code{big.matrix} is returned (for \code{big.matrix} and
\code{filebacked.big.matrix}, and\cr \code{as.big.matrix}),
and \code{TRUE} or \code{FALSE} for \code{is.big.matrix} and the other functions.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{bigmemory}}, and perhaps the class documentation of
\code{\linkS4class{big.matrix}}; \code{\link{attach.big.matrix}} and
\code{\link{describe}}.  Sister packages \pkg{biganalytics}, \pkg{synchronicity},
and \pkg{bigalgebra} provide advanced functionality.
}
\examples{

x <- big.matrix(10, 2, type='integer', init=-5)
options(bigmemory.allow.dimnames=TRUE)
colnames(x) <- c("alpha", "beta")
is.big.matrix(x)
dim(x)
colnames(x)
rownames(x)
x[,]
x[1:8,1] <- 11:18
x[,]

x <- as.big.matrix(matrix(-5, 10, 2))
colnames(x) <- c("alpha", "beta")
is.big.matrix(x)
dim(x)
colnames(x)
rownames(x)
x[1:8,1] <- 11:18
x[,]
apply(x, 1, range)
apply(x, 2, summary)

# The following shared memory example is quite silly, as you wouldn't
# likely do this in a single R session.  But if zdescription were
# passed to another R session via SNOW, NetWorkSpaces, or even by a
# simple file read/write, then the attach.big.matrix() within the
# second R process would give access to the same object in memory.
# Please see the package vignette for real examples.

z <- big.matrix(3, 3, type='integer', init=3)
z[,]
dim(z)
z[1,1] <- 2
z[,]
zdescription <- describe(z)
zdescription
y <- attach.big.matrix(zdescription)
y[,]
y
z
y[1,1] <- -100
y[,]
z[,]

# A short filebacked example, showing the creation of associated files:
files <- dir()
files[grep("example.bin", files)]
z <- filebacked.big.matrix(3, 3, type='integer', init=123,
                           backingfile="example.bin",
                           descriptorfile="example.desc",
                           dimnames=list( c('a','b','c'), c('d', 'e', 'f')))
z[,]
files <- dir()
files[grep("example.bin", files)]
zz <- attach.big.matrix("example.desc")
zz[,]
zz[1,1] <- 0
zzz <- attach.big.matrix(describe(z))
zzz[,]

gc()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{classes}
\keyword{methods}
