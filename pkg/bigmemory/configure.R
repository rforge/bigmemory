cppFlags = 'PKG_CPPFLAGS=-I../inst/include'
pkgLibs = ''

if (Sys.info()[['sysname']] == 'Linux')
{
  pkgLibs = 'PKG_LIBS=-lrt'
}

# Is it a REvo build?
if (Sys.info()[['sysname']] == "Windows" & version$os == 'intel64')
{
  cppFlags = paste(cppFlags, '-GX')
}

if (Sys.info()[['sysname']] == "Linux")
{
  cppFlags = paste(cppFlags, '-DLINUX')
}

configText = cppFlags
if (pkgLibs != '')
{
  configText = paste(pkgLibs, cppFlags, sep="\n")
}
write( configText, 'src/Makevars' )
