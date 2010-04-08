# Eventually, we may have to use the commented lines to specify which
# bigmemory.so file to use.  Right now it looks like it uses the one 
# which is installed.
#library(bigmemory)
#dlls = getLoadedDLLs()
#bmDll = as.character(dlls$bigmemory[2])
#pkgLibs = paste('PKG_LIBS=-lblas -l', bmDll, sep='')

pkgLibs = ""
cppFlags = 'PKG_CPPFLAGS=-I../include'
# This could be put in bigmemory's configure.R file to handle 
# linking to the real-time library.
if (Sys.info()[['sysname']] == 'Linux')
{
  # we need to add -lrt
  pkgLibs='PKG_LIBS=-lrt'
}
write( paste(cppFlags, pkgLibs, sep="\n"), 'src/Makevars' )
