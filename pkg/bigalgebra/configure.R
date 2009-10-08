# Eventually, we may have to use the commented lines to specify which
# bigmemory.so file to use.  Right now it looks like it uses the one 
# which is installed.
#library(bigmemory)
#dlls = getLoadedDLLs()
#bmDll = as.character(dlls$bigmemory[2])
#pkgLibs = paste('PKG_LIBS=-lblas -l', bmDll, sep='')

# Note: I'm not including a license file in Makevars.
pkgLibs = paste('PKG_LIBS=-lblas -llapack ', sep='')
cppFlags = 'PKG_CPPFLAGS=-I../include'
# This could be put in bigmemory's configure.R file to handle 
# linking to the real-time library.
#if (Sys.info()[['sysname']] == 'Linux')
#{
#  # we need to add -lrt
#  pkgLibs=paste(pkgLibs, '-lrt')
#}
write( paste(pkgLibs, "\n", cppFlags, sep=''), 'src/Makevars' )
