
#mmap = function(x, y) {
#  if (is.null(x)) return(NULL)
#  ans <- match(x, y)
#  if (any(is.na(ans))) stop("Couldn't find a match to one of the arguments.")
#  return(ans)
#}

  # - x can be a matrix, data.frame, or big.matrix
  # - if any of the stats are beyond "table" and "summary", then
  # work might be done in parallel (and distributed might need setting)
  # via foreach.
  # - stats must be a named list of lists, each with the first element
  # being "table" or "summary" or the name of a function (not quoted).
  # - length(ccols) must equal length(breaks); NA indicates factor handing,
  # or triplets set up binning.
  # - useNA is the only argument (2nd) to "table"
  # - cols is the 2nd argument (2nd) for "summary", required, followed by na.rm if needed.
  # - other arguments may be specified for further stats, with
  # cols required to be the second argument; all other arguments after
  # cols should either be in order or named appropriately; special functions
  # need to take a matrix as the first argument, so a custom wrapper might be
  # needed for really unusual cases, I suppose.

bigtabulate <- function(x,
                        ccols, breaks=vector("list", length=length(ccols)),
                        stats=list("table", useNA="no"),
                        distributed=FALSE, simplify=TRUE) {

  require(foreach)
  if (is.null(getDoParName())) {
    registerDoSEQ() # A little hack to avoid the foreach warning 1st time.
  }

  if (!is.matrix(x) && !is.big.matrix(x) && !is.data.frame(x))
    stop("condstat requires matrix, data.frame, or big.matrix objects\n")

  if (!is.list(stats[[1]])) stats <- list(stats=stats)
  if (is.null(names(stats))) stop("stats must be a named list")
  if (length(unique(names(stats)))!=length(stats))
    stop("names of stats list must be unique")
  calls <- names(stats)
  if (is.data.frame(x)) {
    for (i in 1:ncol(x)) {
      if (is.character(x[,i])) x[,i] <- factor(x[,i])
      if (is.factor(x[,i])) x[,i] <- as.integer(x[,i])
    }
    x <- as.matrix(x)
  }
  if ( !all(calls %in% c("table", "summary")) ) {
    # Here, we would potentially work in parallel, so make it a big.matrix.
    # So if there is a parallel backend registered, make it a big.matrix,
    # perhaps with an anonymous backing for distribution.
    if (!is.null(getDoParName()) && getDoParName()!="doSEQ")
      x <- as.big.matrix(x, descriptorfile=ifelse(distributed, "", NULL))
  }

  # Check and prepare ccols
  if (length(ccols)!=length(breaks))
    stop("length(ccols) must equal length(breaks).")
  if (!is.numeric(ccols) & !is.character(ccols))
    stop("column indices must be numeric or character vectors.")
  if (is.character(ccols))
    if (is.null(colnames(x))) stop("column names do not exist.")
    else ccols <- mmap(ccols, colnames(x))
  if (!is.numeric(ccols)) ccols <- as.numeric(ccols)

  # Prepare breaks: could be a vector of length(ccols) with numbers of
  # breaks, assumed to span the ranges of the variables, or a list of
  # the same length containing a mixture of numbers of breaks or triplets
  # of (min, max, breaks).  The result of the preparation is a matrix
  # with 3 rows and length(ccols) columns of (min, max, breaks) values.
  breakm <- matrix(NA, 3, length(breaks))
  if (is.numeric(breaks)) {
    if (!is.big.matrix(x)) {
      breakm[1,!is.na(breaks)] <- apply(x[,ccols[!is.na(breaks)], drop=FALSE], 2, min, na.rm=TRUE)
      breakm[2,!is.na(breaks)] <- apply(x[,ccols[!is.na(breaks)], drop=FALSE], 2, max, na.rm=TRUE)
    } else {
      breakm[1,!is.na(breaks)] <- colmin(x, ccols[!is.na(breaks)], na.rm=TRUE)
      breakm[2,!is.na(breaks)] <- colmax(x, ccols[!is.na(breaks)], na.rm=TRUE)
    }
    breakm[3,] <- breaks
  }
  if (is.list(breaks)) {
    for (i in which(!sapply(breaks, is.null))) {
      if (length(breaks[[i]])==1) {
        if (!is.big.matrix(x)) { 
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

  # Prepare stats
  do.table <- FALSE
  table.useNA <- NA
  do.summary <- FALSE
  summary.cols <- NA
  summary.na.rm <- FALSE
  return.map <- FALSE
  fargs <- vector("list", length=0)
  for (i in 1:length(stats)) {
    args <- stats[[i]]
    if (is.function(args[[1]])) {
      return.map <- TRUE
      if (is.character(args[[2]]))
        if (is.null(colnames(x))) stop("column names do not exist.")
        else args[[2]] <- mmap(args[[2]], colnames(x))
      if (!is.numeric(args[[2]])) args[[2]] <- as.numeric(args[[2]])
      fargs[[names(stats)[i]]] <- args
    } else {
      if (args[[1]]=="table") {
        do.table <- TRUE
        table.useNA <- 0
        if (length(args)==2) {
          if (args[[2]]=="ifany") table.useNA <- 1
          if (args[[2]]=="always") table.useNA <- 2
        }
      }
      if (args[[1]]=="summary") {
        do.summary <- TRUE
        if (length(args)==1) stop("additional arguments needed for summary")
        summary.cols <- args[[2]]
        if (length(args)==3) summary.na.rm <- args[[3]]
        if (!is.logical(summary.na.rm)) stop("summary.na.rm must be logical")
        if (!is.numeric(summary.cols) & !is.character(summary.cols))
          stop("column indices must be numeric or character vectors.")
        if (is.character(summary.cols))
          if (is.null(colnames(x))) stop("column names do not exist.")
          else summary.cols <- mmap(summary.cols, colnames(x))
        if (!is.numeric(summary.cols)) summary.cols <- as.numeric(summary.cols)
      }
    }
  }

  if (is.big.matrix(x)) {
    ans <- .Call("BigMatrixTAPPLY", x, ccols, as.numeric(breakm), return.map,
                 do.table, as.integer(table.useNA),
                 do.summary, summary.cols, summary.na.rm)
  } else {
    if (is.integer(x)) {
      ans <- .Call("RIntTAPPLY", x, ccols, as.numeric(breakm), return.map,
                   do.table, as.integer(table.useNA),
                   do.summary, summary.cols, summary.na.rm)
    } else {
      ans <- .Call("RNumericTAPPLY", x, ccols, as.numeric(breakm), return.map,
                   do.table, as.integer(table.useNA),
                   do.summary, summary.cols, summary.na.rm)

      # SPOOF THE iris RETURN FOR TESTING.
      #ans <- NULL
      #ans$table <- table(x[,ccols[1]], x[,ccols[2]])
      #ans$min <- tapply(x[,1], list(x[,ccols[1]], x[,ccols[2]]), min)
      #ans$max <- tapply(x[,1], list(x[,ccols[1]], x[,ccols[2]]), max)
      #ans$mean <- tapply(x[,1], list(x[,ccols[1]], x[,ccols[2]]), mean)
      #ans$sd <- tapply(x[,1], list(x[,ccols[1]], x[,ccols[2]]), sd)
      #ans$NAs <- tapply(x[,1], list(x[,ccols[1]], x[,ccols[2]]), function(x) sum(is.na(x)))
      #ans$levels <- list(levels(factor(x[,ccols[1]])), levels(factor(x[,ccols[2]])))
      #names(ans$levels) <- colnames(x)[ccols]
      #ans$map <- by(1:nrow(x), list(x[,ccols[1]], x[,ccols[2]]),
      #              function(x) return(x))
      #class(ans$map) <- "list"
      #ans$map <- ans$map[1:9]
      #names(ans$map) <- 1:9

    }
  }

  z <- NULL
  dn <- lapply(ans$levels, function(x) { x[is.na(x)] <- "NA"; return(x) })
  ans.table <- NULL
  if (do.table) {
    z$table <- array(ans$table, dim=sapply(dn, length), dimnames=dn)
  }
  ans.summary <- NULL
  if (do.summary) {
    z$min <- array(ans$min, dim=sapply(dn, length), dimnames=dn)
    z$max <- array(ans$max, dim=sapply(dn, length), dimnames=dn)
    z$mean <- array(ans$mean, dim=sapply(dn, length), dimnames=dn)
    z$sd <- array(ans$sd, dim=sapply(dn, length), dimnames=dn)
    z$NAs <- array(ans$NAs, dim=sapply(dn, length), dimnames=dn)
  }

  if (return.map) {
    # Here, process fargs chunkwise.  Use foreach on the chunks.
    xdesc <- if (is.big.matrix(x)) describe(x) else NULL
    fans <- foreach(i=ans$map) %dopar% {
      if (is.null(i)) {
        temp <- as.list(rep(NA, length(fargs)))
        names(temp) <- names(fargs)
        return(temp)
      }
      if (!is.null(xdesc)) {
        y <- attach.big.matrix(xdesc)
        y <- y[i,,drop=FALSE]
      } else {
        y <- x[i,,drop=FALSE]
      }
      temp <- vector("list", length=0)
      for (j in names(fargs)) {
        farg <- fargs[[j]]
        tempname <- names(formals(farg[[1]]))[1]
        if (is.character(farg[[1]])) farg[[1]] <- as.symbol(farg[[1]])
        farg[[2]] <- y[,farg[[2]],drop=FALSE]
        if (!is.null(tempname)) names(farg)[2] <- tempname
        else names(farg)[2] <- ""
        mode(farg) <- "call"
        temp[[j]] <- eval(farg)
      }
      return(temp)
    }

    for (j in names(fargs)) {
      temp <- lapply(fans, function(x) return(x[[j]]))
      if (all(sapply(temp, length)==1) && simplify)
        temp <- array(unlist(temp), dim=sapply(dn, length), dimnames=dn)
      else {
        if (length(unique(sapply(temp, length)))==2) {
          nc <- max(unique(sapply(temp, length)), na.rm=TRUE)
          usenames <- names(temp[[which(sapply(temp, length)==nc)[1]]])
          for (k in which(sapply(temp, length)==1)) {
            if (is.na(temp[[k]])) temp[[k]] <- as.numeric(rep(NA, nc))
            names(temp[[k]]) <- usenames
          }
        }
        temp <- array(temp, dim=sapply(dn, length), dimnames=dn)
      }
      z[[j]] <- temp
    }
  }

  z[is.null(z)] <- NULL
  if (length(z)==1) z <- z[[1]]
  return(z)

}


#x <- iris
#x[,2] <- round(x[,2])

#ans <- bigtabulate( x,
#                    c(2, 5),
#                    stats=list(table=list("table", useNA="no"),
#                               summary=list("summary", cols=c("Sepal.Length", "Petal.Width")),
#                               median=list(median, cols="Petal.Length"),
#                               means=list(colMeans, cols=c("Sepal.Length", "Petal.Length"), na.rm=TRUE),
#                               range=list(range, cols="Petal.Length", na.rm=TRUE)),
#                    distributed=FALSE, simplify=TRUE)




