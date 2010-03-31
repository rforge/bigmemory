# /*
#  *  biganalytics: an R package containing a library of functions for
#  *  use with big.matrix objects of package bigmemory.
#  *
#  *  Copyright (C) 2009 John W. Emerson and Michael J. Kane
#  *
#  *  This file is part of bigmemory.
#  *
#  *  bigmemory is free software; you can redistribute it and/or modify
#  *  it under the terms of the GNU Lesser General Public License as published
#  *  by the Free Software Foundation; either version 3 of the License, or
#  *  (at your option) any later version.
#  *
#  *  This program is distributed in the hope that it will be useful,
#  *  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  *  GNU Lesser General Public License for more details.
#  *
#  *  You should have received a copy of the GNU Lesser General Public License
#  *  along with this program; if not, a copy is available at
#  *  http://www.r-project.org/Licenses/
#  */

# ###########
# FILE: zzz.R
#

.onLoad <- function(libname, pkgname) {
    library.dynam("bigtabulate", pkgname, libname)
}

#.noGenerics <- TRUE

.onUnload <- function(libpath) {
    library.dynam.unload("bigtabulate", libpath)
}
