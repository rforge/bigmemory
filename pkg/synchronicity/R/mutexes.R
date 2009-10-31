# MUTEXES

setClass('mutex')
setGeneric('lock', function(m, ...) standardGeneric('lock'))
setGeneric('lock.shared', function(m, ...) standardGeneric('lock.shared'))
setGeneric('unlock', function(m, ...) standardGeneric('unlock'))
setGeneric('is.timed', function(m) standardGeneric('is.timed'))

setClass('boost.mutex', contains='mutex', 
  representation(isRead='logical', mutexInfoAddr='externalptr'))

setGeneric('read', function(m) standardGeneric('read'))
setMethod('read', signature(m='boost.mutex'), function(m) 
  return(.Call("IsRead", m@mutexInfoAddr)))

setMethod('lock', signature(m='boost.mutex'),
  function(m, ...)
  {
    block = match.call()[['block']]
    if (is.null(block)) block=FALSE
    if (!is.logical(block)) stop('The block argument should be logical')
    return(
      .Call(ifelse(block, 'boost_try_lock', 'boost_lock'), m@mutexInfoAddr))
  })
setMethod('lock.shared', signature(m='boost.mutex'),
  function(m, ...)
  {
    block = match.call()[['block']]
    if (is.null(block)) block=FALSE
    if (!is.logical(block)) stop('The block argument should be logical')
    return( 
      .Call(ifelse(block, 'boost_try_lock_shared', 'boost_lock_shared'),
        m@mutexInfoAddr))
  })
setMethod('unlock', signature(m='boost.mutex'),
  function(m, ...)
  {
    return(
      .Call( ifelse(read(m), 'boost_unlock_shared', 'boost_unlock'),
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

