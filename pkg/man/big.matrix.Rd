\name{big.matrix, filebacked.big.matrix, is.big.matrix, as.big.matrix, is.separated, is.filebacked, backingpath}
\alias{big.matrix}
\alias{filebacked.big.matrix}
\alias{as.big.matrix}
\alias{is.big.matrix}
\alias{is.separated}
\alias{is.filebacked}
\alias{[.big.matrix}
\alias{backingpath}
\title{The core ``big.matrix'' operations.}
\description{
Create a \code{big.matrix} (or check to see if an object is a \code{big.matrix},
or create a \code{big.matrix} from a \code{\link{matrix}}, and so on).  The \code{big.matrix}
may use memory-mapped files (sometimes called file-backing).
}
\usage{
big.matrix(nrow, ncol, type = "integer", init = NULL,
  dimnames = NULL, separated = FALSE, backingfile = NULL,
  backingpath = NULL, descriptorfile = NULL)
filebacked.big.matrix(nrow, ncol, type = "integer", init = NULL, 
  dimnames = NULL, separated = FALSE, backingfile = NULL,
  backingpath = NULL, descriptorfile = NULL)
as.big.matrix(x, type = NULL, separated = FALSE,
  backingfile = NULL, backingpath = NULL, descriptorfile = NULL)
is.big.matrix(x)
is.separated(x)
is.filebacked(x)
backingpath(x)
}
\arguments{
  \item{x}{a \code{matrix} or \code{vector}; if a vector, a one-column \code{big.matrix} is created by \code{as.big.matrix}.}
  \item{nrow}{number of rows.}
  \item{ncol}{number of columns.}
  \item{type}{the type of the atomic element: \code{"double"}, \code{"integer"} (the default), \code{"short"}, or \code{"char"}.}
  \item{init}{a scalar value for initializing the matrix (NULL by default to avoid unnecessary time spent doing the initializing).}
  \item{dimnames}{a list of the row and column names.}
  \item{separated}{use separated column organization of the data; see details.}
  \item{backingfile}{the root name for the file(s) for the cache of \code{x}.}
  \item{backingpath}{the path to the directory containing the file backing cache.}
  \item{descriptorfile}{the name of the file to hold the filebacked description, for subsequent use with \code{\link{attach.big.matrix}}; if NULL, the \code{backingfile} is used as the root.  The descriptor file is placed in the same directory as the backing files.}
}
\details{
A \code{big.matrix} consists of an object in R that does nothing more than point to
the data structure implemented in C++. 
There are two \code{big.matrix} types which manage
data in different ways; both may be shared across separate R processes.
A standard, shared \code{big.matrix} is constrained to available RAM.
A file-backed \code{big.matrix} may
exceed available RAM by using hard drive space, with the memory caching
handled at the operating-system level. The atomic types of these matrices may be
double, integer, short, or char (8, 4, 2, and 1 bytes, respectively).

If \code{x} is a \code{big.matrix}, then \code{x[1:5,]} is returned as an R
\code{matrix} containing the first five rows of \code{x}.  If \code{x} is of type
\code{double}, then the result will be \code{numeric}; otherwise, the result will
be an \code{integer} R matrix.  The expression \code{x} alone
will display information about the R object (e.g. the external pointer) rather
than evaluating the matrix itself (the user should try \code{x[,]} with extreme caution,
recognizing that a huge R \code{matrix} will be created in the process).

If \code{x} has a huge number of rows, for example, then the use of \code{rownames}
will be extremely memory-intensive and should be avoided.  If \code{x} has a huge
number of columns, the user should be cautious when using the \code{separated=TRUE}
option, as this involves overhead of a pointer for each column in the matrix.
Note that row and column names must be set upon creation of the \code{big.matrix},
and may not be modifed.  This restriction relates to the use of shared memory, and
the row and column names are not currently shared across processes (but are passed
in the descriptor (see \code{\link{attach.big.matrix}} and \code{\link{describe}}).

If \code{separated} is \code{TRUE}, then the memory is allocated into separate
vectors for each column.  If \code{separated} is \code{FALSE}, the matrix is
stored in traditional column-major format.  Each may have their advantages
depending on the usage (and possibly depending on yet-to-be-developed code). 
The function \code{is.separated()} queries
the separation type of the \code{big.matrix}.

When a \code{big.matrix}, \code{x}, is passed as an argument
to a function, it is essentially providing call-by-reference rather than
call-by-value behavior.  If the function modifies any of the values of \code{x}
within the function, the changes are not limited in scope to
a local copy within the function.  

A file-backed \code{big.matrix} may exceed available RAM in size by using a file
cache (or possibly multiple file caches, if \code{separated} is \code{TRUE}).
This will incur a performance penalty, obviously.
A side-effect of creating a filei-backed object is
not only the filebacking(s), but a descriptor file (in the same directory) that can
be used for subsequent attachments (see \code{\link{attach.big.matrix}}).
}
\value{
A \code{big.matrix} is returned (for \code{big.matrix} and
\code{filebacked.big.matrix}, and \code{as.big.matrix}),
and \code{TRUE} or \code{FALSE} for \code{is.big.matrix} and the other functions.
}
\author{John W. Emerson and Michael J. Kane}
\seealso{\code{\link{bigmemory}}, and perhaps the class documentation of
\code{\linkS4class{big.matrix}}; \code{\link{attach.big.matrix}} and
\code{\link{describe}}; \code{\link{read.big.matrix}}.
}
\examples{

x <- big.matrix(10, 2, type='integer', init=-5)
colnames(x) = c("alpha", "beta")
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

# The following shared memory example is quite silly, as you wouldn't likely do
# this in a single R session.  But if zdescription were passed to another R session
# via SNOW, NetWorkSpaces, or even by a simple file read/write,
# then the attach.big.matrix() within the second R process would give access to the
# same object in memory.  Please see the package vignette for real examples.
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

# A short filebacked example, showing the creation of associated files and mutexes:
files <- dir()
files[grep("example.bin", files)]
z <- filebacked.big.matrix(3, 3, type='integer', init=123, backingfile="example.bin",
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
