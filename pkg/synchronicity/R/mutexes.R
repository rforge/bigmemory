# MUTEXES

# The virtual class from which mutexe calls will inherit.
setClass('named.exclusive.lock.call')
setGeneric('lock', function(m, ...) standardGeneric('lock'))
setGeneric('try.lock', function(m, ...) standardGeneric('try.lock'))
setGeneric('unlock', function(m, ...) standardGeneric('unlock'))
setGeneric('destroy.resource', function(resourceName) 
  standardGeneric('destroy.resource'))

# This method will work for all of the inherited object types.
setMethod('destroy.resource', signature(resourceName='character'),
  function(resourceName)
  {
    return(.Call('destroy_mutex', resourceName))
  })

setClass('named.sharable.lock.call', contains='named.exclusive.lock.call')
setGeneric('lock.shared', function(m, ...)
  standardGeneric('lock.shared'))
setGeneric('try.lock.shared', function(m, ...)
  standardGeneric('try.lock.shared'))
setGeneric('unlock.shared', function(m, ...)
  standardGeneric('unlock.shared'))

setClass('boost.named.sharable.lock.call', contains='named.sharable.lock.call')

# Note: the only difference in the following functions are the names
# of the functions and the underlying C functions being called.  This
# should be done in a loop with evals!
setMethod('lock', signature(m='boost.named.sharable.lock.call'),
  function(m, ...) 
  {
    name = match.call()[['name']]
    if (is.null(name)) stop('You must specify a resource name.')
    return(.Call('boost_lock', name))
  })
setMethod('try.lock', signature(m='boost.named.sharable.lock.call'),
  function(m, ...)
  {
    name = match.call()[['name']]
    if (is.null(name)) stop('You must specify a resource name.')
    return(.Call('boost_try_lock', name))
  })
setMethod('unlock', signature(m='boost.named.sharable.lock.call'),
  function(m, ...)
  {
    name = match.call()[['name']]
    if (is.null(name)) stop('You must specify a resource name.')
    return(.Call('boost_unlock', name))
  })
setMethod('lock.shared', signature(m='boost.named.sharable.lock.call'),
  function(m, ...)
  {
    name = match.call()[['name']]
    if (is.null(name)) stop('You must specify a resource name.')
    return(.Call('boost_lock_shared', name))
  })
setMethod('try.lock.shared', signature(m='boost.named.sharable.lock.call'),
  function(m, ...)
  {
    name = match.call()[['name']]
    if (is.null(name)) stop('You must specify a resource name.')
    return(.Call('boost_try_lock_shared', name))
  })
setMethod('unlock.shared', signature(m='boost.named.sharable.lock.call'),
  function(m, ...)
  {
    name = match.call()[['name']]
    if (is.null(name)) stop('You must specify a resource name.')
    return(.Call('boost_unlock_shared', name))
  })

# This lock will take the timeout as an intilization argument.  A timeout
# greater than zero must be specified.
check_timeout=function(timeout)
{
  if (timeout <= 0)
    stop("The mutex timeout should be greater than 0")
}

# A mutator/accessor timeout() should be created for getting and setting.
setClass('boost.named.sharable.timed.lock.call', 
  contains='named.sharable.lock.call', representation(timeout='numeric'), 
  prototype=prototype(timeout=-1))

# See above comments.  The following functions should be created in
# a loop with evals.
setMethod('lock', signature(m='boost.named.sharable.timed.lock.call'),
  function(m, ...)
  {
    name = match.call()[['name']]
    if (is.null(name)) stop('You must specify a resource name.')
    check_timeout(m@timeout)
    return(.Call('boost_lock_timed', name, m@timeout))
  })
setMethod('try.lock', signature(m='boost.named.sharable.timed.lock.call'),
  function(m, ...)
  {
    name = match.call()[['name']]
    if (is.null(name)) stop('You must specify a resource name.')
    check_timeout(m@timeout)
    return(.Call('boost_try_lock', name, m@timeout))
  })
setMethod('unlock', signature(m='boost.named.sharable.timed.lock.call'),
  function(m, ...)
  {
    name = match.call()[['name']]
    if (is.null(name)) stop('You must specify a resource name.')
    check_timeout(m@timeout)
    return(.Call('boost_unlock_timed', name))
  })
setMethod('lock.shared', signature(m='boost.named.sharable.timed.lock.call'),
  function(m, ...)
  {
    name = match.call()[['name']]
    if (is.null(name)) stop('You must specify a resource name.')
    check_timeout(m@timeout)
    return(.Call('boost_lock_shared_timed', name, m@timeout))
  })
setMethod('try.lock.shared', 
  signature(m='boost.named.sharable.timed.lock.call'),
  function(m, ...)
  {
    name = match.call()[['name']]
    if (is.null(name)) stop('You must specify a resource name.')
    check_timeout(m@timeout)
    return(.Call('boost_try_shared_lock', name, m@timeout))
  })
setMethod('unlock.shared', 
  signature(m='boost.named.sharable.timed.lock.call'),
  function(m, resourceName)
  {
    name = match.call()[['name']]
    if (is.null(name)) stop('You must specify a resource name.')
    check_timeout(m@timeout)
    return(.Call('boost_unlock_shared_timed', name))
  })

# The constructor of a mutex will need to register it's own finalizer
# via reg.finalizer with exit onexit=TRUE.

#setClass('mutex')
#setGeneric('lock', function(m, block=FALSE) 
#  standardGeneric('lock'))
#setGeneric('lock.shared', function(m, block=FALSE) 
#  standardGeneric('lock.shared'))
#setGeneric('unlock', function(m) standardGeneric('unlock'))

my.ifelse=function(test, yes, no)
{
  if (test) return(yes)
  return(no)
}

#setClass('boost.mutex', contains='mutex', 
#  representation(lockCall='named.sharable.lock.call', isRead='logical',
#    countAddr='externalptr', resourceName='character'))
#setMethod('lock', signature(m='boost.mutex', block='logical'),
#  function(m, block)
#  {
#    m@isRead=FALSE
#    return( my.ifelse(block, lock, try.lock)(m@lockCall, m@resourceName) )
#  })
#setMethod('lock.shared', signature(m='boost.mutex', block='logical'),
#  function(m, block)
#  {
#    m@isRead=TRUE
#    return( my.ifelse(block, lock.shared, try.lock.shared)(m@lockCall,
#      m@resourceName) )
#  })
#setMethod('unlock', signature(m='boost.mutex'),
#  function(m)
#  {
#    return( my.ifelse(m@isRead, unlock.shared, unlock)(m@resourceName) )
#  })

# The constructor for a boost.mutex
#boost.mutex=function()
#{
#  isRead = TRUE
#  resourceName = uuid()
#  countAddr = .Call('CreateSharedCounter', resourceName)
#  return(new('boost.mutex', isRead=isRead, countAddr=countAddr, 
#    resourceName=resourceName))
#}

# A descriptor should return it's own type.

