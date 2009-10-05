\name{lock}
\Rdversion{1.1}
\alias{lock}
\title{Get an exclusive lock on a resource}
\description{Gets a lock on a resource}
\usage{lock(m, name)}
\arguments{
  \item{m}{an object derived from class named.exclusive.lock.call}
  \item{name}{the name of the resource to get the lock from}
}
\value{
the function return TRUE if the lock is successfully called; FALSE otherwise
}
\references{
http://www.boost.org/doc/libs/1_39_0/doc/html/interprocess/synchronization_mechanisms.html#interprocess.synchronization_mechanisms.mutexes
}
\author{Michael Kane}
\examples{
  m = new('boost.named.sharable.lock.call')
  lock(m, 'test_lock_resource1')
  unlock(m, 'test_lock_resource1')
}
