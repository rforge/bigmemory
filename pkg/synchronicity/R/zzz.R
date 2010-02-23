.onLoad <- function(libname, pkgname)
{
  if (!isGeneric('describe'))
    setGeneric('describe', function(x) standardGeneric('describe'))
  
  setMethod('describe', signature(x='boost.mutex'),
    function(x)
    {
      return(new('boost.mutex.descriptor', 
        description=list(shared.name=shared.name(x), 
        timeout=timeout(x))))
    })

  if (!isGeneric('attach.resource'))
  {
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
  }
  setMethod('attach.resource', signature(obj='boost.mutex.descriptor'),
    function(obj, ...)
    {
      desc = description(obj)
      return(boost.mutex(sharedName = desc$shared.name, 
        timeout = desc$timeout))
    })
  library.dynam('synchronicity', pkgname, libname);
}

.onUnload <- function(libpath)
{
  library.dynam.unload('synchronicity', libpath);
}
