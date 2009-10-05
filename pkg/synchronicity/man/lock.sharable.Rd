\name{lock.sharable}
\Rdversion{1.1}
\alias{lock.sharable}
\title{Get a sharable lock on a resource}
\description{Gets a sharable lock on a resource}
\usage{lock.sharable(m, name)}
\arguments{
  \item{m}{an object derived from the class named.sharable.lock.call}
  \item{name}{the name of the resource to get the sharable lock from}
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
  lock.sharable(m, 'test_lock_resource1')
  unlock.sharable(m, 'test_lock_resource1')
}
