\name{bigmemory-package}
\alias{bigmemory-package}
\alias{bigmemory}
\docType{package}
\title{
Manage massive matrices with support for shared memory and memory-mapped files.
}
\description{
Create, store, access, and manipulate massive matrices.  Matrices are,
by default,
allocated to shared memory and may use memory-mapped files.  Packages
\pkg{biganalytics}, \pkg{synchronicity}, \pkg{bigalgebra}, and \pkg{bigtabulate} provide
advanced functionality.  Access to and manipulation of a \code{\link{big.matrix}}
object is exposed in \R by an \acronym{S4} class whose interface is simlar
to that of an \R \code{\link{matrix}}.  Use of these packages in parallel environments
can provide substantial speed and memory efficiencies.
}
\details{
\tabular{ll}{
Package: \tab bigmemory\cr
Type: \tab Package\cr
Version: \tab 4.1.2\cr
Date: \tab 2010-04-07\cr
License: \tab LGPL-3\cr
Copyright: \tab (C) 2010 John W. Emerson and Michael J. Kane\cr
URL: \tab http://www.bigmemory.org\cr
LazyLoad: \tab yes\cr
}

Index of functions/methods (grouped in a friendly way):
\preformatted{
big.matrix, filebacked.big.matrix, as.big.matrix

is.big.matrix, is.separated, is.filebacked

sub.big.matrix, is.sub.big.matrix

dim, dimnames, nrow, ncol, print, head, tail, typeof, length

read.big.matrix, write.big.matrix

mwhich

deepcopy

describe, attach.big.matrix, attach.resource

flush
}

Multi-gigabyte data sets challenge and frustrate \R
users even on well-equipped hardware.
\acronym{C/C++} and \acronym{Fortran} programming can be helpful, but
are cumbersome for interactive data analysis and
lack the flexibility and power of \R's rich statistical
programming environment.  The package \pkg{bigmemory}
and its sister packages
bridge this gap, implementing massive matrices 
and supporting their basic
manipulation and exploration.  A \code{\link{big.matrix}}
is ideal for problems
involving the analysis in \R of manageable subsets of the data,
or when an analysis is conducted mostly in \acronym{C++}. 
The data structures are allocated to shared memory,
allowing separate 
processes on the same computer to share access to a single copy of the
data set.  The data structures may also be file-backed, allowing users
to more easily manage and analyze data sets larger than available
\acronym{RAM} or shared across nodes in a cluster environment.
These features open the door for powerful and
memory-efficient parallel analyses and data mining of massive data sets.

This project (bigmemory and sister packages) is still actively developed.
Please send us an email
letting us know you are trying the package, and we'll keep you
abreast on updates.
}
\note{
Various options are available. \code{options(bigmemory.typecast.warning)}
can be set to avoid annoying warnings that might occur if, for example you
assign \R objects (typically type double) to char, short, or integer
\code{\link{big.matrix}} objects.  
\code{options(bigmemory.print.warning)} protects against extracting and
printing a massive
matrix (which would involve the creation of a second massive copy of the matrix).
\code{options(bigmemory.allow.dimnames)} by default prevents the setting of
\code{dimnames} attributes, because they aren't allocated to shared memory
and changes will not be visible across processes.

Versions >=4.0 represent a major redesign, with the mutexes (locking)
abstracted to package \pkg{synchronicity}, the exploratory data analysis
functionality relocated
to package \pkg{biganalytics}, and new linear algebra support available in
package \pkg{bigalgebra}.  Package \pkg{bigtabulate} provides \code{\link{tapply}}-like
functionality for users while providing a self-contained example for developers interested
in working with \code{big.matrix} objects.  Package \pkg{bigmemory} itself is now minimalist,
providing only the core functionality with no dependencies.

Versions <4.0 supported a limited number of columns
(due to mutex limitations): roughly 50,000 on a typical Linux system.
This restriction has been removed in versions >=4.0.  There
were row limitations (due to a bug that has now been fixed) in versions
<=3.8 of roughly 1 billion, but this has been fixed in versions
>=3.82.

Note that you can't simply use a \code{big.matrix} with many (most) existing
\R functions (e.g. \code{\link{lm}}, \code{\link{kmeans}}).  One nice
exception is \code{\link{split}}, because this function only accesses
subsets of the matrix.
}
\author{
Michael J. Kane and John W. Emerson

Maintainers: Jay Emerson and Mike Kane <bigmemoryauthors@gmail.com>
}
\keyword{package}
\references{The Bigmemory Project: \url{http://www.bigmemory.org/}.}
\seealso{For example,
\code{\link{big.matrix}}, \code{\link{mwhich}}, \code{\link{read.big.matrix}}
}
\examples{
# Our examples are all trivial in size, rather than burning huge amounts
# of memory simply to demonstrate the package functionality.

x <- big.matrix(5, 2, type="integer", init=0,
                dimnames=list(NULL, c("alpha", "beta")))
x
x[1:2,]
x[,1] <- 1:5
x[,"alpha"]
colnames(x)
options(bigmemory.allow.dimnames=TRUE)
colnames(x) <- NULL
x[,]
}
