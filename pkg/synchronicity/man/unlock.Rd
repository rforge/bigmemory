\name{unlock}
\Rdversion{1.1}
\alias{unlock}
\title{unlock an exclusive lock}
\description{removes an exclusive lock on a resource}
\usage{unlock(m, ...)}
\arguments{
  \item{m}{an object derived from class name.exclusive.lock.call}
  \item{...}{options associated with the mutex being used}
}
\value{
the function returs TRUE if the unlock is successfully called; FALSE otherwise
}
\references{
Boost Mutexes
}
\author{Michael Kane}
\examples{
  m = boost.mutex()
  lock(m)
  unlock(m)
}
