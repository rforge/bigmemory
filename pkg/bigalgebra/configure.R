pkgLibs = paste('PKG_LIBS=-lblas -llapack ', sep='')
cppFlags = 'PKG_CPPFLAGS=-I../include'
write( paste(pkgLibs, "\n", cppFlags, sep=''), 'src/Makevars' )
