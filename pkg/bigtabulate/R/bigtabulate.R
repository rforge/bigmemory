bigtabulate <- function(x,
                        ccols, breaks=vector("list", length=length(ccols)),
                        table=TRUE, useNA="no",
                        summary=TRUE, summary.cols=NA, summary.na.rm=FALSE,
                        splitcol=NULL) {

  if (!is.matrix(x) && !is.data.frame(x)) {
    if (class(x)=="big.matrix")
      stop("bigtabulate requires matrix, data.frame, or big.matrix objects.")
  }

  if (is.data.frame(x)) {
    for (i in 1:ncol(x)) {
      if (is.character(x[,i])) x[,i] <- factor(x[,i])
      if (is.factor(x[,i])) x[,i] <- as.integer(x[,i])
    }
    x <- as.matrix(x)
  }

  # Check and prepare ccols
  if (is.logical(ccols)) ccols <- which(ccols)
  if (length(ccols)!=length(breaks))
    stop("length(ccols) must equal length(breaks).")
  if (!is.numeric(ccols) & !is.character(ccols))
    stop("column indices must be numeric or character vectors.")
  if (is.character(ccols))
    if (is.null(colnames(x))) stop("column names do not exist.")
    else ccols <- mmap(ccols, colnames(x))

  # Prepare breaks: could be a vector of length(ccols) with numbers of
  # breaks, assumed to span the ranges of the variables, or a list of
  # the same length containing a mixture of numbers of breaks or triplets
  # of (min, max, breaks).  The result of the preparation is a matrix
  # with 3 rows and length(ccols) columns of (min, max, breaks) values.
  breaks[sapply(breaks, is.null)] <- NA
  breakm <- matrix(NA, 3, length(breaks))
  if (is.numeric(breaks)) {
    if (is.matrix(x)) {
      breakm[1,!is.na(breaks)] <- apply(x[,ccols[!is.na(breaks)], drop=FALSE], 2, min, na.rm=TRUE)
      breakm[2,!is.na(breaks)] <- apply(x[,ccols[!is.na(breaks)], drop=FALSE], 2, max, na.rm=TRUE)
    } else {
      breakm[1,!is.na(breaks)] <- colmin(x, ccols[!is.na(breaks)], na.rm=TRUE)
      breakm[2,!is.na(breaks)] <- colmax(x, ccols[!is.na(breaks)], na.rm=TRUE)
    }
    breakm[3,] <- breaks
  }
  if (is.list(breaks)) {
    for (i in which(!sapply(breaks, is.na))) {
      if (length(breaks[[i]])==1) {
        if (is.matrix(x)) { 
          breakm[1,i] <- min(x[,ccols[i]], na.rm=TRUE)  
          breakm[2,i] <- max(x[,ccols[i]], na.rm=TRUE)
        } else {
          breakm[1,i] <- colmin(x, ccols[i], na.rm=TRUE)
          breakm[2,i] <- colmax(x, ccols[i], na.rm=TRUE)
        }
        breakm[3,i] <- breaks[[i]]
      } else {
        breakm[,i] <- breaks[[i]]
      }
    }
  }

  if (!is.logical(table)) stop("table must be logical.")
  table.useNA <- -1
  if (useNA=="no") table.useNA <- 0
  if (useNA=="ifany") table.useNA <- 1
  if (useNA=="always) table.useNA <- 2
  if (table.useNA==-1) stop("invalid argument to useNA.")

  if (!is.logical(summary)) stop("summary must be logical.")
  if (!is.logical(summary.na.rm)) stop("summary.na.rm must be logical.")
  if (is.logical(summary.cols)) summary.cols <- which(summary.cols)
  if (!is.numeric(summary.cols) && !is.character(summary.cols))
    stop("column indices must be numeric or character vectors.")
  if (is.character(summary.cols))
    if (is.null(colnames(x))) stop("column names do not exist.")
    else summary.cols <- mmap(summary.cols, colnames(x))
  if (!is.null(splitcol)) {
    if (!is.na(splitcol)) {
      if (!is.numeric(splitcol) & !is.character(splitcol))
        stop("splitcol must be numeric or character specifying one column, or NA or NULL.")
      if (is.character(splitcol))
        if (is.null(colnames(x))) stop("column names do not exist.")
        else splitcol <- mmap(splitcol, colnames(x))
      if (length(splitcol)!=1) stop("splitcol must be length 1.")
    }
  }

  if (!is.matrix(x)) {
    ans <- .Call("BigMatrixTAPPLY", x, as.numeric(ccols), as.numeric(breakm),
                 as.logical(table), as.integer(table.useNA),
                 as.logical(summary), as.numeric(summary.cols), as.logical(summary.na.rm),
                 as.numeric(splitcol))
  } else {
    if (is.integer(x)) {
      ans <- .Call("RIntTAPPLY", x, as.numeric(ccols), as.numeric(breakm),
                   as.logical(table), as.integer(table.useNA),
                   as.logical(summary), as.numeric(summary.cols), as.logical(summary.na.rm),
                   as.numeric(splitcol))
    } else {
      ans <- .Call("RNumericTAPPLY", x, as.numeric(ccols), as.numeric(breakm),
                   as.logical(table), as.integer(table.useNA),
                   as.logical(summary), as.numeric(summary.cols), as.logical(summary.na.rm),
                   as.numeric(splitcol))
    }
  }

  # The return will always contain
  # - ans$levels, a list of length(ccols) of factor levels possibly plus "NA"

  # It may contain:
  # - ans$table:	vector of length prod(dim())
  # - ans$summary:	list of length prod(dim()) of cell summary matrices with 5 columns
  # - ans$split:	list of length prod(dim()) containing the split or map result


  z <- NULL
  dn <- lapply(ans$levels, function(x) { x[is.na(x)] <- "NA"; return(x) })

  if (table) z$table <- array(ans$table, dim=sapply(dn, length), dimnames=dn)

  if (summary) z$summary <- array(ans$summary, dim=sapply(dn, length), dimnames=dn)
 
  if (!is.null(splitcol)) {
    z$split <- ans$split
    names(z$split) <- names(dn)
  }

  if (length(z)==1) return(z[[1]])
  return(z)

}


#                        stats=list("table", useNA="no"),
#                        distributed=FALSE, simplify=TRUE) {

#  require(foreach)
#  if (is.null(getDoParName())) {
#    registerDoSEQ() # A little hack to avoid the foreach warning 1st time.
#  }

#  if (!is.list(stats[[1]])) stats <- list(stats=stats)
#  if (is.null(names(stats))) stop("stats must be a named list")
#  if (length(unique(names(stats)))!=length(stats))
#    stop("names of stats list must be unique")
#  calls <- names(stats)

#  if ( !all(calls %in% c("table", "summary")) ) {
#    # Here, we would potentially work in parallel, so make it a big.matrix.
#    # So if there is a parallel backend registered, make it a big.matrix,
#    # perhaps with an anonymous backing for distribution.
#    if (!is.null(getDoParName()) && getDoParName()!="doSEQ") {
#      require(bigmemory)
#      if (is.matrix(x)) {
#        x <- as.big.matrix(x, backingfile=ifelse(distributed, "", NULL))
#        warning("Temporary shared big.matrix created for parallel calculations.")
#      }
#      if (!is.shared(x) || !is.filebacked(x)) {
#        x <- deepcopy(x, backingfile=ifelse(distributed, "", NULL))
#        warning("Temporary shared big.matrix created for parallel calculations.")
#      }
#    }
#  }

#  # Prepare stats
#  do.table <- FALSE
#  table.useNA <- NA
#  do.summary <- FALSE
#  summary.cols <- NA
#  summary.na.rm <- FALSE
#  return.map <- FALSE
#  fargs <- vector("list", length=0)
#  for (i in 1:length(stats)) {
#    args <- stats[[i]]
#    if (is.function(args[[1]])) {
#      return.map <- TRUE
#      if (is.character(args[[2]]))
#        if (is.null(colnames(x))) stop("column names do not exist.")
#        else args[[2]] <- mmap(args[[2]], colnames(x))
#      if (!is.numeric(args[[2]])) args[[2]] <- as.numeric(args[[2]])
#      fargs[[names(stats)[i]]] <- args
#    } else {
#      if (args[[1]]=="table") {
#        do.table <- TRUE
#        table.useNA <- 0
#        if (length(args)==2) {
#          if (args[[2]]=="ifany") table.useNA <- 1
#          if (args[[2]]=="always") table.useNA <- 2
#        }
#      }
#      if (args[[1]]=="summary") {
#        do.summary <- TRUE
#        if (length(args)==1) stop("additional arguments needed for summary")
#        summary.cols <- args[[2]]
#        if (length(args)==3) summary.na.rm <- args[[3]]
#        if (!is.logical(summary.na.rm)) stop("summary.na.rm must be logical")
#        if (!is.numeric(summary.cols) & !is.character(summary.cols))
#          stop("column indices must be numeric or character vectors.")
#        if (is.character(summary.cols))
#          if (is.null(colnames(x))) stop("column names do not exist.")
#          else summary.cols <- mmap(summary.cols, colnames(x))
#        if (!is.numeric(summary.cols)) summary.cols <- as.numeric(summary.cols)
#      }
#    }
#  }


#  ans.table <- NULL
#  ans.summary <- NULL

#  if (return.map) {
#    # Here, process fargs chunkwise.  Use foreach on the chunks.
#    xdesc <- if (!is.matrix(x)) describe(x) else NULL
#    fans <- foreach(i=ans$map) %dopar% {
#      if (is.null(i)) {
#        temp <- as.list(rep(NA, length(fargs)))
#        names(temp) <- names(fargs)
#        return(temp)
#      }
#      if (!is.null(xdesc)) {
#        y <- attach.big.matrix(xdesc)
#        y <- y[i,,drop=FALSE]
#      } else {
#        y <- x[i,,drop=FALSE]
#      }
#      temp <- vector("list", length=0)
#      for (j in names(fargs)) {
#        farg <- fargs[[j]]
#        tempname <- names(formals(farg[[1]]))[1]
#        if (is.character(farg[[1]])) farg[[1]] <- as.symbol(farg[[1]])
#        farg[[2]] <- y[,farg[[2]],drop=FALSE]
#        if (!is.null(tempname)) names(farg)[2] <- tempname
#        else names(farg)[2] <- ""
#        mode(farg) <- "call"
#        temp[[j]] <- eval(farg)
#      }
#      return(temp)
#    }

#    for (j in names(fargs)) {
#      temp <- lapply(fans, function(x) return(x[[j]]))
#      if (all(sapply(temp, length)==1) && simplify)
#        temp <- array(unlist(temp), dim=sapply(dn, length), dimnames=dn)
#      else {
#        if (length(unique(sapply(temp, length)))==2) {
#          nc <- max(unique(sapply(temp, length)), na.rm=TRUE)
#          usenames <- names(temp[[which(sapply(temp, length)==nc)[1]]])
#          for (k in which(sapply(temp, length)==1)) {
#            if (is.na(temp[[k]])) temp[[k]] <- as.numeric(rep(NA, nc))
#            names(temp[[k]]) <- usenames
#          }
#        }
#        temp <- array(temp, dim=sapply(dn, length), dimnames=dn)
#      }
#      z[[j]] <- temp
#    }
#  }

#  z[is.null(z)] <- NULL



