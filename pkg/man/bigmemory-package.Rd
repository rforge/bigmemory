\name{bigmemory-package}
\alias{bigmemory-package}
\alias{bigmemory}
\docType{package}
\title{
Manage massive matrices with support for shared memory and memory-mapped files.
}
\description{
Create, store, access, and manipulate massive matrices.  Matrices are
allocated to shared memory and may use memory-mapped files (sometimes
called file-backing).  Packages biganalytics, synchronicity, and bigalgebra
provide advanced functionality. Access to and manipulation
of a \code{big.matrix} object is exposed in R by an S4 
class whose interface is simlar to an R \code{matrix}.
}
\details{
\tabular{ll}{
Package: \tab bigmemory\cr
Type: \tab Package\cr
Version: \tab 4.0\cr
Date: \tab 2009-10-03\cr
License: \tab LGPL-3\cr
}

Multi-gigabyte data sets challenge and frustrate R
users even on well-equipped hardware.
C/C++ and Fortran programming can be helpful, but
are cumbersome for interactive data analysis and
lack the flexibility and power of R's rich statistical
programming environment.  The package \code{bigmemory}
and its sister packages
bridge this gap, implementing massive matrices 
and supporting their basic
manipulation and exploration.  A \code{\link{big.matrix}}
is ideal for problems
involving the analysis in R of manageable subsets of the data,
or when an analysis is conducted mostly in C++. 
The data structures are allocated to shared memory, 
allowing separate 
processes on the same computer to share access to a single copy of the
data set.  The data structures may also be file-backed, allowing users
to more easily manage and analyze data sets larger than available RAM.
These features open the door for powerful and
memory-efficient parallel analyses and data mining of massive data sets.
}
\note{
Global options may be of interest:
\code{options(bigmemory.typecast.warning)} is available and can
be set to avoid annoying warnings that might occur if, for example you
assign R objects (typically type double) to char, short, or integer
\code{\link{big.matrix}} objects; \code{options(bigmemory.print.warning)}
is available to warn against actually printing a massive object.

Versions 4.0 and beyond represent a major redesign, with the mutexes (locking)
abstracted to package \code{synchronicity}, the EDA functionality relocated
to package \code{biganalytics}, and new linear algebra support available in
package \code{bigalgebra}.  Package \code{bigmemory} itself is now minimalist,
providing only the core functionality with no dependencies.

Versions 3.X and earlier support a limited number of columns
(due to mutex limitations), roughly 50,000 on a typical Linux system.
This restriction has been removed in versions 4.0 and beyond.  There
were row limitations (due to a bug that has now been fixed) in versions
3.8 and earlier of roughly 1 billion, but this has been fixed in versions
3.82 and later.  We apologize for the inconvenience, and appreciate
and and all feedback.

This package is still actively developed.  Please send us an email
letting us know you are trying the package, and we'll keep you
abreast on updates. - Jay and Mike
}
\author{
John W. Emerson and Michael J. Kane

Maintainer: Jay and Mike <bigmemoryauthors@gmail.com>
}
\keyword{package}
\references{See \url{http://www.bigmemory.org}.}
\seealso{For example,
\code{\link{big.matrix}}, \code{\link{mwhich}}, \code{\link{read.big.matrix}}
}
\examples{
# Our examples are all trivial in size, rather than burning huge amounts
# of memory simply to demonstrate the package functionality.

x <- big.matrix(5, 2, type="integer", init=0)
colnames(x)=c("alpha", "beta")
x
x[1:2,]
x[,1] <- 1:5
x[,"alpha"]
}
