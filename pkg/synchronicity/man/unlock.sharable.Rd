\name{unlock.sharable}
\Rdversion{1.1}
\alias{unlock.sharable}
\title{unlock a sharable lock}
\description{removes a sharable lock on a resource}
\usage{unlock.sharable(m, name)}
\arguments{
  \item{m}{an object derived from class named.sharable.lock.call}
  \item{name}{the name of the resource to unlock}
}
\value{
the function returs TRUE if the unlock is successfully called; FALSE otherwise
}
\references{
http://www.boost.org/doc/libs/1_39_0/doc/html/interprocess/synchronization_mechanisms.html#interprocess.synchronization_mechanisms.mutexes
}
\author{
Michael Kane
}
\examples{
  m = new('boost.named.sharable.lock.call')
  lock.sharable(m, 'test_lock_resource1')
  unlock.sharable(m, 'test_lock_resource1')
}
