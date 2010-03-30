pkgLibs=""
# Find the bigmemory directory
bigmemoryDirNames = paste(.libPaths(), "/bigmemory/include", sep='')
isDir = file.info(bigmemoryDirNames)$isdir
isDir[is.na(isDir)] = FALSE
# use the first one 
isDir = min(which(isDir))
if (length(isDir) == 0)
{
  stop("Could not find bigmemory install directory")
}
cppFlags = paste('PKG_CPPFLAGS=-I../include -I', bigmemoryDirNames[isDir],
  sep="")
write( paste(pkgLibs, "\n", cppFlags, sep=''), 'src/Makevars' )

