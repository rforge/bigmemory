cppFlags = 'PKG_CPPFLAGS=-I../inst/include'
pkgLibs = ''

if (Sys.info()[['sysname']] == 'Linux')
{
  pkgLibs = 'PKG_LIBS=-lrt'
}

configText = cppFlags
if (pkgLibs != '')
{
  configText = paste(pkgLibs, cppFlags, sep="\n")
}
write( configText, 'src/Makevars' )
