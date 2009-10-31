fix_path = function(path)
{
  if (is.null(path) || path == '')
  {
    path = ''
    return(path)
  }
  else if (substr(path, nchar(path), nchar(path))!='/')
  {
    if (is.na(file.info(path)$isdir))
      stop("The supplied backing path does not exist.")
    path= paste(path, '/', sep='')
  }
  else 
  {
    if ( is.na(file.info( substr(path, 1, nchar(path)-1) )) )
      stop( "The supplied backing path does not exist.")
  }
  return(path)
}

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

setGeneric('shared.name', function(m) standardGeneric('shared.name'))

setMethod('shared.name', signature(m='boost.mutex'), 
  function(m) 
  {
    return(.Call('GetResourceName', m@mutexInfoAddr))
  })

setGeneric('timeout', function(m) standardGeneric('timeout'))

setMethod('timeout', signature(m='boost.mutex'),
  function(m)
  {
    return(.Call('GetTimeout', m))
  })
# The constructor for a boost.mutex
boost.mutex=function(sharedName=NULL, timeout=NULL)
{
  isRead = TRUE
  if (is.null(sharedName)) 
  {
    sharedName = uuid()
  }
  if (!is.null(timeout) && !is.numeric(timeout))
  {
    stop("The timeout parameter must be numeric.")
  }
  if (!is.null(timeout) && timeout <= 0)
  {
    stop("You must specify a timeout greater than zero.")
  }
  mutexInfoAddr=.Call('CreateBoostMutexInfo', sharedName, as.double(timeout))
  return(new('boost.mutex', isRead=isRead, mutexInfoAddr=mutexInfoAddr))
}


setClass('descriptor', representation(description='list'))
setGeneric('describe', function(x) standardGeneric('describe'))
setGeneric('description', function(x) standardGeneric('description'))
setMethod('description', signature(x='descriptor'),
  function(x) return(x@description))

setClass('boost.mutex.descriptor', contains='descriptor')

setMethod('describe', signature(x='boost.mutex'),
  function(x)
  {
    return(new('boost.mutex.descriptor', 
      description=list(shared.name=shared.name(x), timeout=timeout(x))))
  })

setGeneric('attach.resource', function(obj, ...) 
  standardGeneric('attach.resource'))

setMethod('attach.resource', signature(obj='character'),
  function(obj, ...)
  {
    path = match.call()[['path']]
    if (is.null(path))
    {
      path = ''
    }
    info <- dget(paste(fix_path(path), obj, sep=""))
    return(attach.resource(info))
  })

setMethod('attach.resource', signature(obj='boost.mutex.descriptor'),
  function(obj, ...)
  {
    desc = description(obj)
    return(boost.mutex(sharedName = desc$shared.name, timeout = desc$timeout))
  })
