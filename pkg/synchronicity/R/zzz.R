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
          path <- '.'
        }
        path <- path.expand(path)
        if (basename(obj) != obj)
        {

            warning(paste("Two paths were specified in attach.resource.",
              "The one associated with the file will be used.", sep="  "))
          path <- dirname(obj)
          obj <- basename(obj)
        }
        fileWithPath <- file.path(path, obj)
        fi = file.info(fileWithPath)
        print(dir())
        if (is.na(fi$isdir))
          stop( paste("The file", fileWithPath, "could not be found") )
        if (fi$isdir)
          stop( fileWithPath, "is a directory" )
        info <- dget(fileWithPath)
        return(attach.resource(info, path=path))
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
