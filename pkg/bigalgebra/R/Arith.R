#
# + need trivial operators?  (See Matrix)
#

## Some ``Univariate'' "Arith":
# This is surprising, it's triggered by    > +x     ?
setMethod("+", signature(e1 = "big.matrix", e2 = "missing"), function(e1) {
  cat("testing odd +big.matrix\n")
  return(e1)
  })


## "fallback":
#setMethod("-", signature(e1 = "big.matrix", e2 = "missing"),
#	  function(e1) {
#	      warning("inefficient method used for \"- e1\"")
#	      0-e1
#	  })


#
# + : if a big.matrix is involved, return a similar big.matrix non-destructively.
#

setMethod("+", signature(e1="big.matrix", e2="big.matrix"),
  function(e1, e2) {
    if (typeof(e1)!="double" || typeof(e2)!="double")
      stop("bigalgebra currently only supports numeric (double) operations")
    if (nrow(e1)==nrow(e2) && ncol(e1)==ncol(e2)) {
      bf <- NULL
      if (is.filebacked(e1) || is.filebacked(e2)) bf <- ""
      ans <- deepcopy(e2, backingfile=bf)
      daxpy(NULL, 1, e1, 1, ans, 1)
      return(ans)
    } else stop("non-conformable arrays")
  })

setMethod("+", signature(e1="matrix", e2="big.matrix"),
  function(e1, e2) {
    if (!is.double(e1) || typeof(e2)!="double")
      stop("bigalgebra currently only supports numeric (double) operations")
    if (nrow(e1)==nrow(e2) && ncol(e1)==ncol(e2)) {
      bf <- NULL
      if (is.filebacked(e2)) bf <- ""
      ans <- deepcopy(e2, backingfile=bf)
      daxpy(NULL, 1, e1, 1, ans, 1)
      return(ans)
    } else stop("non-conformable arrays")
  })

setMethod("+", signature(e1="big.matrix", e2="matrix"),
  function(e1, e2) e2+e1)



setMethod("+", signature(e1="vector", e2="big.matrix"),
  function(e1, e2) {
    if (length(e1)!=1 || !is.numeric(e1))
      stop("e1 would need to be a numeric scalar")
    #if (length(e2)/length(e1) != round(length(e2)/length(e1))) {
    #  warning("longer object length is not a multiple of shorter object length")
    #}
    if (is.integer(e1)) e1 <- as.numeric(e1)
    if (typeof(e2)!="double")
      stop("bigalgebra currently only supports numeric (double) operations")
    bf <- NULL
    if (is.filebacked(e2)) bf <- ""
    if (length(e1)==1) {
      ans <- big.matrix(nrow(e2), ncol(e2), type="double", init=e1, backingfile=bf)
    } else {
      ans <- big.matrix(nrow(e2), ncol(e2), type="double", backingfile=bf)
      ans[,] <- e1
    }
    daxpy(NULL, 1, e2, 1, ans, 1)
    return(ans)
  })

setMethod("+", signature(e1="big.matrix", e2="vector"),
  function(e1, e2) e2+e1)





