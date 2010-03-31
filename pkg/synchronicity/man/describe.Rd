\name{describe, attach.resource}
\Rdversion{1.1}
\alias{describe}
\alias{attach.resource}
\title{Create Descriptors to Mutexes and Attach}
\description{
The \code{describe} function returns information that is needed to 
``connect'' to a mutex from another process.  This connection is performed
by the \code{attach.resource} function.}
\usage{
describe(x)
attach.resource(obj, \dots)}
\arguments{
  \item{x}{a \code{boost.mutex} object}
  \item{obj}{a \code{boost.mutex.descriptor} object}
  \item{\dots}{options associated with the mutex being used including
\code{path} which gives the directory where a \code{boost.mutex.descriptor}
object has been stored via dput. }
}
\value{
The \code{describe} function returns a \code{boost.mutex.descriptor} object.
the \code{attach.resource} function returns a \code{boost.mutex} object
described by \code{obj}.
}
\author{Michael J. Kane <kaneplusplus@gmail.com>}
\examples{
  m = boost.mutex()
  mm = attach.resource(describe(m))
  # Now, both m and mm specify the same mutex.
}
\keyword{programming}
\keyword{misc}
