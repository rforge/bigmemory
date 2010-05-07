cppFlags <- 'PKG_CPPFLAGS=-I../inst/include'
pkgLibs <- ''

if (Sys.info()[['sysname']] == 'Linux')
{
  pkgLibs <- 'PKG_LIBS=-lrt'
  cppFlags <- paste(cppFlags, '-DLINUX')
}

# Is it a REvo build?
# If we want to build in windows, we should simply modify the
# src/Makevars.win file to include -GX and not include the
# INTERLOCKED_EXCHANGEHACK thingy.
#if (Sys.info()[['sysname']] == "Windows" & version$os == 'intel64')
#{
#  cppFlags <- paste(cppFlags, '-GX')
#}

configText <- paste(cppFlags, pkgLibs, sep="\n")
write( configText, 'src/Makevars' )
