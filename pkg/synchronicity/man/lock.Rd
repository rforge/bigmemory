\name{lock}
\Rdversion{1.1}
\alias{lock}
\title{Get an exclusive lock on a resource}
\description{Gets a lock on a resource}
\usage{lock(m, ...)}
\arguments{
  \item{m}{an object derived from class named.exclusive.lock.call}
  \item{\dots}{options associated with the mutex being used}
}
\value{
the function return TRUE if the lock is successfully called; FALSE otherwise
}
\references{
Boost mutexes
}
\author{Michael Kane}
\examples{
  m = boost.mutex()
  lock(m)
  unlock(m)
}
