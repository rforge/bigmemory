
setGeneric('binit', function(x, cols, breaks=10)
  standardGeneric('binit'))

setMethod('binit', signature(x='matrix'),
  function(x, cols, breaks=10)
  {
    print("Some minor code modifications because of colmi and colmax")
    print("Could be avoided, but let's leave it for now.")

    stop("Not yet implemented.")
  })

setMethod('binit', signature(x='big.matrix'),
  function(x, cols, breaks=10)
  {
    cols <- bigmemory:::cleanupcols(cols, ncol(x), colnames(x))
    if (length(cols)<1 || length(cols)>2) {
      stop("Error in binit: only 1 or 2 columns is supported.")
    }
    if ( !is.list(breaks) && (length(breaks)==1 || length(breaks)==2) ) {
      if (is.numeric(breaks)) {
        usebreaks <- breaks
      } else { stop("Error in binit: breaks must be numeric.\n") }
      if (length(cols)==1) {
        breaks <- c(colmin(x, cols, na.rm=TRUE),
                    colmax(x, cols, na.rm=TRUE), usebreaks[1])
      }
      if (length(cols)==2) {
        if (length(usebreaks)==1) usebreaks <- c(usebreaks, usebreaks)
        mins <- colmin(x, cols, na.rm=TRUE)
        maxs <- colmax(x, cols, na.rm=TRUE)
        breaks <- list(c(mins[1], maxs[1], usebreaks[1]),
                       c(mins[2], maxs[2], usebreaks[2]))
      }
    }
    if (!is.list(breaks) && length(breaks)!=3)
      stop("Error in binit: incorrect specification of breaks.")
    if (is.list(breaks) & (length(breaks)!=2 ||
                           length(breaks[[1]])!=3 ||
                           length(breaks[[2]])!=3))
      stop("Error in binit: serious breaks problem.")

    if (is.list(breaks)) {
      ret = .Call("CBinItmain2", x@address,
        as.double(cols), as.double(breaks[[1]]), as.double(breaks[[2]]))
      ret <- matrix(ret, breaks[[1]][3], breaks[[2]][3])
      rb <- seq(breaks[[1]][1], breaks[[1]][2], length.out=breaks[[1]][3]+1)
      rnames <- (rb[-length(rb)] + rb[-1]) / 2
      cb <- seq(breaks[[2]][1], breaks[[2]][2], length.out=breaks[[2]][3]+1)
      cnames <- (cb[-length(cb)] + cb[-1]) / 2
      ret <- list(counts=ret, rowcenters=rnames, colcenters=cnames,
                  rowbreaks=rb, colbreaks=cb)
    } else {
      ret = .Call("CBinItmain1", x@address,
        as.double(cols), as.double(breaks))
      b <- seq(breaks[1], breaks[2], length.out=breaks[3]+1)
      rnames <- (b[-length(b)] + b[-1]) / 2
      ret <- list(counts=ret, centers=rnames, breaks=b)
    }

    return(ret)
  })


