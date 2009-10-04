cppFlags = 'PKG_CPPFLAGS=-I../include'
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
