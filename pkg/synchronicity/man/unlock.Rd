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
http://www.boost.org/doc/libs/1_39_0/doc/html/interprocess/synchronization_mechanisms.html#interprocess.synchronization_mechanisms.mutexes
}
\author{Michael Kane}
\examples{
  m = new('boost.named.sharable.lock.call')
  lock(m, name='test_lock_resource1')
  unlock(m, name='test_lock_resource1')
  destroy.resource('test_lock_resource1')
}
