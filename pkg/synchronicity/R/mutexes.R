# MUTEXES

# The virtual class from which mutex calls will inherit.
#setClass('named.exclusive.lock.call')
#setGeneric('destroy.resource', function(resourceName) 
#  standardGeneric('destroy.resource'))

# This method will work for all of the inherited object types.
#setMethod('destroy.resource', signature(resourceName='character'),
#  function(resourceName)
#  {
#    return(.Call('destroy_mutex', resourceName))
#  })

#setClass('named.sharable.lock.call', contains='named.exclusive.lock.call')
#setGeneric('lock.shared', function(m, ...)
#  standardGeneric('lock.shared'))
#setGeneric('try.lock.shared', function(m, ...)
#  standardGeneric('try.lock.shared'))
#setGeneric('unlock.shared', function(m, ...)
#  standardGeneric('unlock.shared'))

#setClass('boost.named.sharable.lock.call', contains='named.sharable.lock.call')

# Note: the only difference in the following functions are the names
# of the functions and the underlying C functions being called.  This
# should be done in a loop with evals!
#shared.lock.fun.string=function( rFunctionName, cFunctionName )
#{
#  return(paste(
#    "setMethod('", rFunctionName, "'",
#    ", signature(m='boost.named.sharable.lock.call'),",
#    "function(m, ...)",
#    "{",
#      "name = match.call()[['name']];",
#      "if (is.null(name)) stop('You must specify a resource name.');",
#      "return(.Call('", cFunctionName, "', name))",
#    "})", sep=''))
#}
#eval(parse(text=shared.lock.fun.string('lock', 'boost_lock')))
#eval(parse(text=shared.lock.fun.string('try.lock', 'boost_try_lock')))
#eval(parse(text=shared.lock.fun.string('unlock', 'boost_unlock')))
#eval(parse(text=shared.lock.fun.string('lock.shared', 'boost_lock_shared')))
#eval(parse(text=shared.lock.fun.string('try.lock.shared', 
#  'boost_try_lock_shared')))
#eval(parse(text=shared.lock.fun.string('unlock.shared', 'boost_unlock_shared')))

# This lock will take the timeout as an intilization argument.  A timeout
# greater than zero must be specified.
#check_timeout=function(timeout)
#{
#  if (timeout <= 0)
#    stop("The mutex timeout should be greater than 0")
#}

# A mutator/accessor timeout() should be created for getting and setting.
#setClass('boost.named.sharable.timed.lock.call', 
#  contains='named.sharable.lock.call', representation(timeout='numeric'), 
#  prototype=prototype(timeout=-1))

#shared.timed.lock.fun.string=function( rFunctionName, cFunctionName )
#{
#  return(paste(
#    "setMethod('", rFunctionName, "'",
#    ", signature(m='boost.named.sharable.timed.lock.call'),",
#    "function(m, ...)",
#    "{",
#      "name = match.call()[['name']];",
#      "if (is.null(name)) stop('You must specify a resource name.');",
#      "check_timeout(m@timeout);",
#      "return(.Call('", cFunctionName, "', name, m@timeout))",
#    "})", sep=''))
#}
#eval(parse(text=shared.timed.lock.fun.string('lock', 'boost_lock_timed')))
#eval(parse(text=shared.timed.lock.fun.string('try.lock',
#  'boost_try_lock_timed')))
#eval(parse(text=shared.timed.lock.fun.string('unlock', 'boost_unlock_timed')))
#eval(parse(text=shared.timed.lock.fun.string('lock.shared',
#  'boost_unlock_timed')))
#eval(parse(text=shared.timed.lock.fun.string('try.lock.shared', 
#  'boost_try_shared_lock')))
#eval(parse(text=shared.timed.lock.fun.string('unlock.shared', 
#  'boost_unlock_shared_timed')))


#my.ifelse=function(test, yes, no)
#{
#  if (test) return(yes)
#  return(no)
#}

setClass('mutex')
setGeneric('lock', function(m, ...) standardGeneric('lock'))
setGeneric('lock.shared', function(m, ...) standardGeneric('lock.shared'))
setGeneric('unlock', function(m, ...) standardGeneric('unlock'))
setGeneric('is.timed', function(m) standardGeneric('is.timed'))

setClass('boost.mutex', contains='mutex', 
  representation(isRead='logical', mutexInfoAddr='externalptr'))
setMethod('lock', signature(m='boost.mutex'),
  function(m, ...)
  {
    block = match.call()[['block']]
    if (is.null(block)) block=FALSE
    if (!is.logical(block)) stop('The block argument should be logical')
    m@isRead=FALSE
    return(
      .Call(ifelse(block, 'boost_try_lock', 'boost_lock'), m@mutexInfoAddr))
  })
setMethod('lock.shared', signature(m='boost.mutex'),
  function(m, ...)
  {
    block = match.call()[['block']]
    if (is.null(block)) block=FALSE
    if (!is.logical(block)) stop('The block argument should be logical')
    m@isRead=TRUE
    return( 
      .Call(ifelse(block, 'boost_lock_shared', 'boost_try_lock_shared'),
        m@mutexInfoAdddr))
#    return( my.ifelse(block, lock.shared, try.lock.shared)(m@lockCall,
#      resource.name(m)) )
  })
setMethod('unlock', signature(m='boost.mutex'),
  function(m, ...)
  {
#    return( my.ifelse(m@isRead, unlock.shared, unlock)(resource.name(m)) )
    return(
      .Call( ifelse(m@isRead, 'boost_unlock_shared', 'boost_unlock'),
        m@mutexInfoAddr) )
  })

setGeneric('resource.name', function(m) standardGeneric('resource.name'))

setMethod('resource.name', signature(m='boost.mutex'), 
  function(m) 
  {
    return(.Call('GetResourceName', m@mutexInfoAddr))
  })

# The constructor for a boost.mutex
boost.mutex=function(resourceName=NULL, timeout=NULL)
{
  isRead = TRUE
  if (is.null(resourceName)) 
  {
    resourceName = uuid()
  }
  if (!is.null(timeout) && !is.numeric(timeout))
  {
    stop("The timeout parameter must be numeric.")
  }
  if (!is.null(timeout) && timeout <= 0)
  {
    stop("You must specify a timeout greater than zero.")
  }
  mutexInfoAddr=.Call('CreateBoostMutexInfo', resourceName, as.double(timeout))
  return(new('boost.mutex', isRead=isRead, mutexInfoAddr=mutexInfoAddr))
}

