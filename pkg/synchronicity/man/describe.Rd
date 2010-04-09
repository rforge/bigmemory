\name{describe, attach.mutex}
\Rdversion{1.1}
\alias{describe}
\alias{attach.mutex}
\title{Create Descriptors to Mutexes and Attach}
\description{
The \code{describe} function returns information that is needed to 
``connect'' to a mutex from another process.  This connection is performed
by the \code{attach.mutex} function.}
\usage{
describe(x)
attach.mutex(obj, \dots)}
\arguments{
  \item{x}{a \code{boost.mutex} object}
  \item{obj}{a \code{boost.mutex.descriptor} object}
  \item{\dots}{options associated with the mutex being used including
\code{path} which gives the directory where a \code{boost.mutex.descriptor}
object has been stored via dput. }
}
\value{
The \code{describe} function returns a \code{boost.mutex.descriptor} object.
the \code{attach.mutex} function returns a \code{boost.mutex} object
described by \code{obj}.
}
\author{Michael J. Kane <kaneplusplus@gmail.com>}
\examples{
  m = boost.mutex()
  mm = attach.mutex(describe(m))
  # Now, both m and mm specify the same mutex.
}
\keyword{programming}
\keyword{misc}
