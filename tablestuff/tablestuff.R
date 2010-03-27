#
# March 27, 2010
#

# length(x) should be prod(dim(x))!  Oops.
# Hey!  Check this out: split(x, x[,2]), where x is a big.matrix.

table(...)    # Skip for now, get it for free.

condstat(x, ccols, breaks=vector("list", length=length(ccols)),
         stats=list(c("table", na.rm=FALSE),
                    c("summary", col=NULL, na.rm=FALSE),
                    c(FUN1, cols1=NULL, ...),
                    c(FUN2, cols2=NULL, ...),
                    ...)

.C(x, ccols,
   breaks (in some nice way),
   table (boolean),
   table.na.rm (boolean),
   summary (boolean),
   col (integer column),
   summary.na.rm (boolean),
   return.map (boolean))

RETURNS: faclevs = list of length(ccols) of vectors of sorted levels

	optionally if return.map==TRUE:

		list of length=prod(dim(ar)) of vectors of row indices.

	ar if table is done
	4 arrays if summaries are done

# Unrelated: sort by column


   

