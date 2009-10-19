#############################################################################
# This function is used to match up a vector of column names to the
# entire set of column names, providing the proper column indices.
# The name choice was based on the phrase "multiple map" though
# perhaps we should have made a different choice.

mmap = function(x, y) {
  if (is.null(x)) return(NULL)
  ans <- match(x, y)
  if (any(is.na(ans))) stop("Couldn't find a match to one of the arguments.")
  return(ans)
}

#############################################################################

setClass("big.matrix", representation(address='externalptr'))

big.matrix <- function(nrow, ncol, type='integer', init=NULL,
                       dimnames=NULL, separated=FALSE, backingfile=NULL,
                       backingpath=NULL, descriptorfile=NULL)
{
  if (!is.null(backingfile))
  {
    return(filebacked.big.matrix(nrow=nrow, ncol=ncol, type=type, init=init,
      dimnames=dimnames, separated=separated, backingfile=backingfile,
      backingpath=backingpath, descriptorfile=descriptorfile))
  }
  if (nrow < 1 | ncol < 1)
    stop('A big.matrix must have at least one row and one column')

  typeVal <- NULL
  if (type == 'integer') typeVal <- 4
  if (type == 'double') typeVal <- 8
  if (type == 'short') typeVal <- 2
  if (type == 'char') typeVal <- 1
  if (is.null(typeVal)) stop('invalid type')
  if (!is.null(dimnames)) {
    rownames <- dimnames[[1]]
    colnames <- dimnames[[2]]
  } else {
    rownames <- NULL
    colnames <- NULL
  }
  address <- .Call('CCreateSharedMatrix', as.double(nrow),
    as.double(ncol), as.character(colnames), as.character(rownames),
    as.integer(typeVal), as.double(init), as.logical(separated))
  if (is.null(address)) {
    stop(paste("Error: memory could not be allocated for instance",
               "of type big.matrix", sep=' '))
  }
  x <- new("big.matrix", address=address)
  if (is.null(x)) {
    stop("Error encountered when creating instance of type big.matrix")
  }
  return(x)
}



setGeneric('is.big.matrix', function(x) standardGeneric('is.big.matrix'))

setMethod('is.big.matrix', signature(x='big.matrix'),
  function(x) return(TRUE))

setMethod('is.big.matrix', definition=function(x) return(FALSE))

setGeneric('as.big.matrix', 
  function(x, type=NULL, separated=FALSE,
    backingfile=NULL, backingpath=NULL,
    descriptorfile=NULL) standardGeneric('as.big.matrix'))

setMethod('as.big.matrix', signature(x='matrix'),
  function(x, type, separated, backingfile, backingpath, descriptorfile)
  {
    if (!is.numeric(x)) {
      warning("Casting to numeric type")
      x <- matrix( as.numeric(x), nrow=nrow(x), dimnames=dimnames(x) )
    }
    if (is.null(type)) type <- typeof(x)

    if (type=="integer" | type=="double" | type=="short" | type=="char") 
    {
      if (!is.null(backingfile)) {
        y <- big.matrix(nrow=nrow(x), ncol=ncol(x), type=type, 
                        init=NULL, dimnames=dimnames(x), separated=separated,
                        backingfile=backingfile, backingpath=backingpath,
                        descriptorfile=descriptorfile)
      } else {
        y <- big.matrix(nrow=nrow(x), ncol=ncol(x), type=type, init=NULL, 
                        dimnames=dimnames(x), separated=separated)
      }
      y[1:nrow(x),1:ncol(x)] <- x
      junk <- gc() 
    } else stop('bigmemory: that type is not implemented.')
    return(y)
  })

setMethod('as.big.matrix', signature(x='vector'),
  function(x, type, separated, backingfile, backingpath, descriptorfile)
  {
    x <- matrix(x, length(x), 1)
    warning("Coercing vector to a single-column matrix.")
    return(as.big.matrix(x, type, separated, backingfile, 
                         backingpath, descriptorfile))
  })
  
colnames.bm <- function(x)
{
  ret <- .Call("GetColumnNamesBM", x@address)
  if (length(ret)==0) return(NULL)
  return(ret)
}

rownames.bm <- function(x)
{
  ret <- .Call("GetRowNamesBM", x@address)
  if (length(ret)==0) return(NULL)
  return(ret)
}

assign('colnames.bm<-', 
  function(x, value) {
      if (is.character(value)) {
        if (any(value=="")) {
          value <- NULL
          warning("empty strings prohibited in column names")
        }
      } else {
        if (!is.null(value)) {
          value <- as.character(value)
          warning("column names coerced to character")
        }
      }
      if (!is.null(value) & length(value) != ncol(x))
        stop("length of 'colnames' not equal to array extent.")
      .Call("SetColumnNames", x@address, value)
      return(x)
  })

assign('rownames.bm<-',
  function(x,value) {
      if (is.character(value)) {
        if (any(value=="")) {
          value = NULL
          warning("empty strings prohibited in row names")
        }
      } else {
        if (!is.null(value)) {
          value <- as.character(value)
          warning("row names coerced to character")
        }
      }
      if (length(value) != nrow(x) & !is.null(value)) 
        stop("length of 'rownames' not equal to array extent.")
      .Call("SetRowNames", x@address, value)
      return(x)
  })

setMethod('ncol', signature(x="big.matrix"),
  function(x) {
    return(.Call("CGetNcol", x@address))
  })

setMethod('nrow', signature(x="big.matrix"), 
  function(x) {
    return(.Call("CGetNrow", x@address))
  })

setMethod('dim', signature(x="big.matrix"),
  function(x) return(c(nrow(x), ncol(x))))

GetElements.bm <- function(x, i, j, drop=TRUE)
{
  if (!is.numeric(i) & !is.character(i) & !is.logical(i))
    stop("row indices must be numeric, logical, or character vectors.")
  if (!is.numeric(j) & !is.character(j) & !is.logical(j))
    stop("column indices must be numeric, logical, or character vectors.")
  if (is.character(i))
    if (is.null(rownames(x))) stop("row names do not exist.")
    else i <- mmap(i, rownames(x))
  if (is.character(j))
    if (is.null(colnames(x))) stop("column names do not exist.")
    else j <- mmap(j, colnames(x))
  if (is.logical(i)) {
    if (length(i) != nrow(x))
      stop("row vector length must match the number of rows of the matrix.")
    i <- which(i)
  }
  if (is.logical(j)) {
    if (length(j) != ncol(x))
      stop(paste("column vector length must match the number of",
                 "columns of the matrix."))
    j <- which(j)
  }

  tempi <- .Call("CCleanIndices", as.double(i), as.double(nrow(x)))
  if (is.null(tempi[[1]])) stop("Illegal row index usage in extraction.\n")
  if (tempi[[1]]) i <- tempi[[2]]
  tempj <- .Call("CCleanIndices", as.double(j), as.double(ncol(x)))
  if (is.null(tempj[[1]])) stop("Illegal column index usage in extraction.\n")
  if (tempj[[1]]) j <- tempj[[2]]

  retList <- .Call("GetMatrixElements", x@address, as.double(j), as.double(i))

  dimnames(retList[[1]]) <- list( retList[[2]], retList[[3]] )
  if (drop) {
    if (any(dim(retList[[1]])==1)) {
      if (dim(retList[[1]])[1]!=1 || dim(retList[[1]])[2]!=1) {
        if (dim(retList[[1]])[1]==1) {
          thesenames <- retList[[3]]
        } else thesenames <- retList[[2]]
      } else thesenames <- NULL
      retList[[1]] = as.vector(retList[[1]])
      names(retList[[1]]) <- thesenames
    }
  }
  return(retList[[1]])
}

GetCols.bm <- function(x, j, drop=TRUE)
{
  if (!is.numeric(j) & !is.character(j) & !is.logical(j))
    stop("column indices must be numeric, logical, or character vectors.")
  if (is.character(j))
    if (is.null(colnames(x))) stop("column names do not exist.")
    else j <- mmap(j, colnames(x))
  if (is.logical(j)) {
    if (length(j) != ncol(x))
      stop(paste("column vector length must match the number of",
                 "columns of the matrix."))
    j <- which(j)
  }

  tempj <- .Call("CCleanIndices", as.double(j), as.double(ncol(x)))
  if (is.null(tempj[[1]])) stop("Illegal column index usage in extraction.\n")
  if (tempj[[1]]) j <- tempj[[2]]

  retList <- .Call("GetMatrixCols", x@address, as.double(j))

  dimnames(retList[[1]]) <- list( retList[[2]], retList[[3]] )
  if (drop) {
    if (any(dim(retList[[1]])==1)) {
      if (dim(retList[[1]])[1]!=1 || dim(retList[[1]])[2]!=1) {
        if (dim(retList[[1]])[1]==1) {
          thesenames <- retList[[3]]
        } else thesenames <- retList[[2]]
      } else thesenames <- NULL
      retList[[1]] = as.vector(retList[[1]])
      names(retList[[1]]) <- thesenames
    }
  }
  return(retList[[1]])
}

GetRows.bm <- function(x, i, drop=TRUE)
{
  if (!is.numeric(i) & !is.character(i) & !is.logical(i))
    stop("row indices must be numeric, logical, or character vectors.")
  if (is.character(i))
    if (is.null(rownames(x))) stop("row names do not exist.")
    else i <- mmap(i, rownames(x))
  if (is.logical(i)) {
    if (length(i) != nrow(x))
      stop("row vector length must match the number of rows of the matrix.")
    i <- which(i)
  }
  tempi <- .Call("CCleanIndices", as.double(i), as.double(nrow(x)))
  if (is.null(tempi[[1]])) stop("Illegal row index usage in extraction.\n")
  if (tempi[[1]]) i <- tempi[[2]]

  retList <- .Call("GetMatrixRows", x@address, as.double(i))

  dimnames(retList[[1]]) <- list( retList[[2]], retList[[3]] )
  if (drop) {
    if (any(dim(retList[[1]])==1)) {
      if (dim(retList[[1]])[1]!=1 || dim(retList[[1]])[2]!=1) {
        if (dim(retList[[1]])[1]==1) {
          thesenames <- retList[[3]]
        } else thesenames <- retList[[2]]
      } else thesenames <- NULL
      retList[[1]] = as.vector(retList[[1]])
      names(retList[[1]]) <- thesenames
    }
  }
  return(retList[[1]])
}

GetAll.bm <- function(x, drop=TRUE)
{
  # Note here the locks are handled in the signature, because there
  # is no index cleaning to be done.

  retList <- .Call("GetMatrixAll", x@address)

  dimnames(retList[[1]]) <- list( retList[[2]], retList[[3]] )
  if (drop) {
    if (any(dim(retList[[1]])==1)) {
      if (dim(retList[[1]])[1]!=1 || dim(retList[[1]])[2]!=1) {
        if (dim(retList[[1]])[1]==1) {
          thesenames <- retList[[3]]
        } else thesenames <- retList[[2]]
      } else thesenames <- NULL
      retList[[1]] = as.vector(retList[[1]])
      names(retList[[1]]) <- thesenames
    }
  }
  return(retList[[1]])
}

setMethod("[",
  signature(x = "big.matrix", drop = "missing"),
  function(x, i, j) {
    return(GetElements.bm(x, i, j))
  })

setMethod("[",
  signature(x = "big.matrix", drop = "logical"),
  function(x, i, j, drop) {
    return(GetElements.bm(x, i, j, drop))
  })

setMethod("[",
  signature(x = "big.matrix", i="missing", drop = "missing"),
  function(x, j) {
    return(GetCols.bm(x, j))
  })

setMethod("[",
  signature(x = "big.matrix", i="missing", drop = "logical"),
  function(x, j, drop) {
    return(GetCols.bm(x, j, drop))
  })

setMethod("[",
  signature(x = "big.matrix", j="missing", drop = "missing"),
  function(x, i) {
    return(GetRows.bm(x, i))
  })

setMethod("[",
  signature(x = "big.matrix", j="missing", drop = "logical"),
  function(x, i, drop) {
    return(GetRows.bm(x, i, drop))
  })

# Because we don't have any index checking/fixing, we do our locking here.
setMethod("[",
  signature(x = "big.matrix", i="missing", j="missing", drop = "missing"),
  function(x) {
    ret <- GetAll.bm(x)
    return(ret)
})

# Because we don't have any index checking/fixing, we do our locking here.
setMethod("[",
  signature(x = "big.matrix", i="missing", j="missing", drop = "logical"),
  function(x, drop) {
    ret <- GetAll.bm(x, drop)
    return(ret)
})

SetElements.bm <- function(x, i, j, value)
{
  if (!is.numeric(i) & !is.character(i) & !is.logical(i))
    stop("row indices must be numeric, logical, or character vectors.")
  if (!is.numeric(j) & !is.character(j) & !is.logical(j))
    stop("column indices must be numeric, logical, or character vectors.")
  if (is.character(i))
    if (is.null(rownames(x))) stop("row names do not exist.")
    else i <- mmap(i, rownames(x))
  if (is.character(j))
    if (is.null(colnames(x))) stop("column names do not exist.")
    else j <- mmap(j, colnames(x))
  if (is.logical(i)) {
    if (length(i) != nrow(x))
      stop("row vector length must match the number of rows of the matrix.")
    i <- which(i)
  }
  if (is.logical(j)) {
    if (length(j) != ncol(x))
      stop(paste("column vector length must match the number of",
                 "columns of the matrix."))
    j <- which(j)
  }

  tempi <- .Call("CCleanIndices", as.double(i), as.double(nrow(x)))
  if (is.null(tempi[[1]])) stop("Illegal row index usage in extraction.\n")
  if (tempi[[1]]) i <- tempi[[2]]
  tempj <- .Call("CCleanIndices", as.double(j), as.double(ncol(x)))
  if (is.null(tempj[[1]])) stop("Illegal column index usage in extraction.\n")
  if (tempj[[1]]) j <- tempj[[2]]

  if ( options()$bigmemory.typecast.warning &&
       ((typeof(value) == "double") && (typeof(x) != "double") ||
       (typeof(value) == "integer" &&
        (typeof(x) != "double" && typeof(x) != "integer"))) )
  {
    warning(cat("Assignment will down cast from ", typeof(value), " to ",
                typeof(x), "\nHint: To remove this warning type:  ",
                "options(bigmemory.typecast.warning=FALSE)\n", sep=''))

  # Note: i may be a mwhich statement in which case we _must_ ensure
  # that we disable read locking before it is evaluated or we will
  # have a race condition.  - Jay and Mike.
  }

  totalts <- length(i) * length(j)
  # If we are assigning from a matrix, make sure the dimensions agree.
  if (is.matrix(value))
  {
    if (ncol(value) != length(j) | nrow(value) != length(i)) 
    {
      stop("Matrix dimensions do not agree with big.matrix instance set size.")
    }
  } else if (length(value) != totalts) {
    # Otherwise, make sure we are assigning the correct number of things
    # (rep if necessary)
    numReps <- totalts / length(value)
    if (numReps != round(numReps)) 
    {
      stop(paste("number of items to replace is not a multiple of",
                 "replacement length"))
    }
  }
  if (typeof(x) != 'double') {
    integerVals = na.omit(as.integer(value))
    if ( sum(integerVals == na.omit(as.integer(value))) !=
         length(integerVals) | is.factor(value)) {
      warning("non-integer (possibly Inf or -Inf) typecast to integer")
    }
  }
  # Note: we pass doubles as doubles, but anything else as integers.
  if (typeof(x) == 'double') {
    .Call("SetMatrixElements", x@address, as.double(j), as.double(i), 
			as.double(value))
  } else {
    .Call("SetMatrixElements", x@address, as.double(j), as.double(i), 
			as.integer(value))
  }
  return(x)
}

SetCols.bm <- function(x, j, value)
{
  if (!is.numeric(j) & !is.character(j) & !is.logical(j))
    stop("column indices must be numeric, logical, or character vectors.")
  if (is.character(j))
    if (is.null(colnames(x))) stop("column names do not exist.")
    else j <- mmap(j, colnames(x))
  if (is.logical(j)) {
    if (length(j) != ncol(x))
      stop(paste("column vector length must match the number of",
                 "columns of the matrix."))
    j <- which(j)
  }

  tempj <- .Call("CCleanIndices", as.double(j), as.double(ncol(x)))
  if (is.null(tempj[[1]])) stop("Illegal column index usage in extraction.\n")
  if (tempj[[1]]) j <- tempj[[2]]

  if ( options()$bigmemory.typecast.warning &&
       ((typeof(value) == "double") && (typeof(x) != "double") ||
       (typeof(value) == "integer" &&
        (typeof(x) != "double" && typeof(x) != "integer"))) )
    warning(cat("Assignment will down cast from ", typeof(value), " to ",
                typeof(x), "\nHint: To remove this warning type:  ",
                "options(bigmemory.typecast.warning=FALSE)\n", sep=''))

  # Note: i may be a mwhich statement in which case we _must_ ensure
  # that we disable read locking before it is evaluated or we will
  # have a race condition.  - Jay and Mike.

  totalts <- nrow(x) * length(j)
  # If we are assigning from a matrix, make sure the dimensions agree.
  if (is.matrix(value)){
    if (ncol(value) != length(j) | nrow(value) != nrow(x)) 
    {
      stop("Matrix dimensions do not agree with big.matrix instance set size.")
    }
  } 
  else if (length(value) != totalts) 
  {
    # Otherwise, make sure we are assigning the correct number of things
    # (rep if necessary)
    numReps <- totalts / length(value)
    if (numReps != round(numReps)) {
      stop(paste("number of items to replace is not a multiple of",
                 "replacement length"))
    }
  }
  if (typeof(x) != 'double') {
    integerVals = na.omit(as.integer(value))
    if ( sum(integerVals == na.omit(as.integer(value))) !=
         length(integerVals) | is.factor(value)) {
      warning("non-integer (possibly Inf or -Inf) typecast to integer")
    }
  }
  # Note: we pass doubles as doubles, but anything else as integers.
  if (typeof(x) == 'double') 
  {
    .Call("SetMatrixCols", x@address, as.double(j), as.double(value))
  } 
  else 
  {
    .Call("SetMatrixCols", x@address, as.double(j), as.integer(value))
  }
  return(x)
}

SetRows.bm <- function(x, i, value) 
{
  if (!is.numeric(i) & !is.character(i) & !is.logical(i))
    stop("row indices must be numeric, logical, or character vectors.")
  if (is.character(i))
    if (is.null(rownames(x))) stop("row names do not exist.")
    else i <- mmap(i, rownames(x))
  if (is.logical(i)) {
    if (length(i) != nrow(x))
      stop("row vector length must match the number of rows of the matrix.")
    i <- which(i)
  }

  tempi <- .Call("CCleanIndices", as.double(i), as.double(nrow(x)))
  if (is.null(tempi[[1]])) stop("Illegal row index usage in extraction.\n")
  if (tempi[[1]]) i <- tempi[[2]]

  if ( options()$bigmemory.typecast.warning &&
       ((typeof(value) == "double") && (typeof(x) != "double") ||
       (typeof(value) == "integer" &&
        (typeof(x) != "double" && typeof(x) != "integer"))) )
    warning(cat("Assignment will down cast from ", typeof(value), " to ",
                typeof(x), "\nHint: To remove this warning type:  ",
                "options(bigmemory.typecast.warning=FALSE)\n", sep=''))

  # Note: i may be a mwhich statement in which case we _must_ ensure
  # that we disable read locking before it is evaluated or we will
  # have a race condition.  - Jay and Mike.

  totalts <- length(i) * ncol(x)
  # If we are assigning from a matrix, make sure the dimensions agree.
  if (is.matrix(value))
  {
    if (ncol(value) != ncol(x) | nrow(value) != length(i)) 
    {
      stop("Matrix dimensions do not agree with big.matrix instance set size.")
    }
  } 
  else if (length(value) != totalts) 
  {
    # Otherwise, make sure we are assigning the correct number of things
    # (rep if necessary)
    numReps <- totalts / length(value)
    if (numReps != round(numReps)) 
    {
      stop(paste("number of items to replace is not a multiple of",
                 "replacement length"))
    }
  }
  if (typeof(x) != 'double') 
  {
    integerVals = na.omit(as.integer(value))
    if ( sum(integerVals == na.omit(as.integer(value))) !=
         length(integerVals) | is.factor(value)) {
      warning("non-integer (possibly Inf or -Inf) typecast to integer")
    }
  }
  # Note: we pass doubles as doubles, but anything else as integers.
  if (typeof(x) == 'double') {
    .Call("SetMatrixRows", x@address, as.double(i), as.double(value))
  } 
  else 
  {
    .Call("SetMatrixRows", x@address, as.double(i), as.integer(value))
  }
  return(x)
}

SetAll.bm <- function(x, value) 
{
  if ( options()$bigmemory.typecast.warning &&
       ((typeof(value) == "double") && (typeof(x) != "double") ||
       (typeof(value) == "integer" &&
        (typeof(x) != "double" && typeof(x) != "integer"))) )
    warning(cat("Assignment will down cast from ", typeof(value), " to ",
                typeof(x), "\nHint: To remove this warning type:  ",
                "options(bigmemory.typecast.warning=FALSE)\n", sep=''))

  # Note: i may be a mwhich statement in which case we _must_ ensure
  # that we disable read locking before it is evaluated or we will
  # have a race condition.  - Jay and Mike.

  totalts <- nrow(x) * ncol(x)
  # If we are assigning from a matrix, make sure the dimensions agree.
  if (is.matrix(value))
  {
    if (ncol(value) != ncol(x) | nrow(value) != nrow(x)) 
    {
      stop("Matrix dimensions do not agree with big.matrix instance set size.")
    }
  } 
  else if (length(value) != totalts) 
  {
    # Otherwise, make sure we are assigning the correct number of things
    # (rep if necessary)
    numReps <- totalts / length(value)
    if (numReps != round(numReps)) {
      stop(paste("number of items to replace is not a multiple of", 
                 "replacement length"))
    }
  }
  if (typeof(x) != 'double') 
  {
    integerVals = na.omit(as.integer(value))
    if ( sum(integerVals == na.omit(as.integer(value))) !=
         length(integerVals) | is.factor(value)) 
    {
      warning("non-integer (possibly Inf or -Inf) typecast to integer")
    }
  }
  # Note: we pass doubles as doubles, but anything else as integers.
  if (typeof(x) == 'double') 
  {
    .Call("SetMatrixAll", x@address, as.double(value))
  } 
  else 
  {
    .Call("SetMatrixAll", x@address, as.integer(value))
  }
  return(x)
}

setMethod('[<-',
  signature(x = "big.matrix"),
  function(x, i, j, value) {
    return(SetElements.bm(x, i, j, value))
  })

setMethod('[<-',
  signature(x = "big.matrix", i="missing"),
  function(x, j, value) {
    return(SetCols.bm(x, j, value))
  })

setMethod('[<-',
  signature(x = "big.matrix", j="missing"),
  function(x, i, value) {
    return(SetRows.bm(x, i, value))
  })

setMethod('[<-',
  signature(x = "big.matrix", i="missing", j="missing"),
  function(x, value) {
    return(SetAll.bm(x, value))
  })

setMethod('typeof', signature(x="big.matrix"),
  function(x) return(.Call('GetTypeString', x@address)))

setMethod('head', signature(x="big.matrix"),
  function(x, n = 6) {
    n <- min(as.integer(n), nrow(x))
    if (n<1 | n>nrow(x)) stop("n must be between 1 and nrow(x)")
    return(x[1:n,])
  })

setMethod('tail', signature(x="big.matrix"),
  function(x, n = 6) {
    n <- min(as.integer(n), nrow(x))
    if (n<1 | n>nrow(x)) stop("n must be between 1 and nrow(x)")
    return(x[(nrow(x)-n+1):nrow(x),])
  })

setMethod('print', signature(x='big.matrix'), 
  function(x) {
    if (options()$bigmemory.print.warning==TRUE)
    {
      cat("Warning: This is not advised.  Here is the head of the matrix:\n")
      print(head(x))
    }
    else
    {
      # Should change this to a C print function, unfortunately, for proper
      # formatting, this means we would also have to pass the terminal
      # width.
      print(x[,])
    }
  })

###################################################################
# mwhich()
#
# x big.matrix  
# cols  is.numeric or is.character
# vals  list of scalar or 2-vectors otherwise
# comps could be missing, in which case we'll fill in 'eq' in signature,
#       a list of comparisons matching dim of associated vals component

setGeneric('mwhich', function(x, cols, vals, comps, op = 'AND')
  standardGeneric('mwhich'))

setMethod('mwhich',
  signature(x='big.matrix', op='character'),
  function(x, cols, vals, comps, op) {
    return(mwhich.internal(x, cols, vals, comps, op, 'MWhichBigMatrix'))
  })

setMethod('mwhich',
  signature(x='matrix', op='character'),
  function(x, cols, vals, comps, op)
  {
    if (is.integer(x))
      return(mwhich.internal(x, cols, vals, comps, op, 'MWhichRIntMatrix'))
    if (is.numeric(x))
      return(mwhich.internal(x, cols, vals, comps, op, 'MWhichRNumericMatrix'))
    stop("Unsupported matrix type given to mwhich")
  })

setMethod('mwhich',
  signature(x='big.matrix', op='missing'),
  function(x, cols, vals, comps)
    return(mwhich.internal(x, cols, vals, comps, op='OR', 
                           whichFuncName='MWhichBigMatrix')))

setMethod('mwhich',
  signature(x='matrix', op='missing'),
  function(x, cols, vals, comps)
  {
    if (is.integer(x))
      return(mwhich.internal(x, cols, vals, comps, op='OR', 
                             whichFuncName='MWhichRIntMatrix'))
    if (is.numeric(x))
      return(mwhich.internal(x, cols, vals, comps, op='OR', 
                             whichFuncName='MWhichRNumericMatrix'))
    stop("Unsupported matrix type given to mwhich")
  })

mwhich.internal <- function(x, cols, vals, comps, op, whichFuncName) 
{
  cols <- cleanupcols(cols, ncol(x), colnames(x))
  if (length(setdiff(cols, 1:ncol(x))) > 0)
    stop('Invalid column(s) in which()')

  # if vals or comps are not lists but are length 1 or 2, make them
  # trivial lists.
  if ( !is.list(vals) & 
       (length(vals)==1 || length(vals)==2) ) {
    vals <- list(vals)
  } else {
    if (!is.list(vals)) stop('vals should be a list')
  }
  if ( !is.list(comps) &
       (length(comps)==1 || length(comps)==2)) {
    comps <- list(comps)
  } else {
    if (!is.list(comps)) stop('comps should be a list')
  }

  # Replicate vals or comps if appropriate.
  if (length(cols)!=length(vals)) {
    if (length(vals)==1) {
      vals <- data.frame(matrix(unlist(vals), length(vals), length(cols)))
    } else stop('length(vals) must be 1 or length(cols)')
  }
  if (length(cols)!=length(comps)) {
    if (length(comps)==1) {
      comps <- data.frame(matrix(unlist(comps), length(comps), length(cols)),
                          stringsAsFactors=FALSE)
    } else stop('length(comps) must be 1 or length(cols)')
  }
  if (length(comps)!=length(vals))
    stop('length of comps must equal length of vals')
  if (any(!unlist(lapply(comps, is.character))) ||
      any(!(unlist(comps) %in% c('eq', 'neq', 'le', 'lt', 'ge', 'gt')))) {
    stop('comps must contain eq, neq, le, lt, ge, or gt')
  }

  testCol <- cols
  opVal <- 0
  if (op == 'OR') opVal <- 1
  minVal <- rep(NA, length(cols))
  maxVal <- rep(NA, length(cols))
  chkmin <- rep(0, length(cols))
  chkmax <- rep(0, length(cols))

  for (i in 1:length(cols)) {

    if (length(vals[[i]])==1) {
      # Here, we have the easy comparisons.
      if (is.na(vals[[i]]) && (comps[[i]]!='eq' && comps[[i]]!='neq'))
        stop('NA comparison limited to eq and neq, not le, lt, gt, or ge')
      if (length(comps[[i]])==1) {
        if (comps[[i]]=='eq' || comps[[i]]=='neq') {
          minVal[i] <- vals[[i]]
          maxVal[i] <- vals[[i]]
        }
        if (comps[[i]]=='neq') {
          chkmin[i] <- -1
          chkmax[i] <- -1            # Not used, but....
        }
        if (comps[[i]]=='ge' || comps[[i]]=='gt') {
          minVal[i] <- vals[[i]]
          maxVal[i] <- Inf
          if (comps[[i]]=='gt') chkmin[i] <- 1
        }
        if (comps[[i]]=='le' || comps[[i]]=='lt') {
          minVal[i] <- -Inf
          maxVal[i] <- vals[[i]]
          if (comps[[i]]=='lt') chkmax[i] <- 1
        }
      } else stop('vals/comps must be componentwise of same dimension')
    } else {
      # Here, we have two vals and two comps
      if (any(is.na(vals[[i]]))) stop('NAs not allowed in interval comparison')
      minVal[i] <- vals[[i]][1]
      maxVal[i] <- vals[[i]][2]
      if (comps[[i]][1]=='gt') chkmin[i] <- 1
      if (comps[[i]][2]=='lt') chkmax[i] <- 1
      if (comps[[i]][1]!='gt' && comps[[i]][1]!='ge')
        stop('invalid comparison of lower bound')
      if (comps[[i]][2]!='lt' && comps[[i]][2]!='le')
        stop('invalid comparison of upper bound')
    }

  } # End of the for loop

  ##### The new C function has new vectors chkmin and chkmax;
  ##### the value 0 indicates comparison with equality,
  ##### the value 1 indicates a strict inequality,
  ##### the value -1 indicates a 'neq' check;
  ##### if is.na checking is required, only the minVal needs to be
  ##### used, with chkmin = 0 being is.na and chkmin = 1 being !is.na.

  ret = NULL
  if (whichFuncName == 'MWhichBigMatrix')
    ret = .Call(whichFuncName, x@address, as.double(testCol), 
                as.double(minVal), as.double(maxVal), 
                as.integer(chkmin), as.integer(chkmax), as.integer(opVal))
  else
    ret = .Call(whichFuncName, x, nrow(x),
                as.double(testCol), 
                as.double(minVal), as.double(maxVal), 
                as.integer(chkmin), as.integer(chkmax), as.integer(opVal))

  return(ret)
}

setMethod('dimnames', signature(x = "big.matrix"),
  function(x) return(list(rownames.bm(x), colnames.bm(x))))

setMethod('dimnames<-', signature(x = "big.matrix", value='list'),
  function(x, value) {
    rownames.bm(x) <- value[[1]]
    colnames.bm(x) <- value[[2]]
    warning("We are here.\n")
    return(x)
  })


setGeneric('read.big.matrix', 
  function(fileName, sep=',', header=FALSE, row.names=NULL, 
    col.names=NULL, type=NA, skip=0, separated=FALSE, 
    backingfile=NULL, backingpath=NULL, descriptorfile=NULL, 
    extraCols=NULL) standardGeneric('read.big.matrix'))

setMethod('read.big.matrix', signature(fileName='character'),
  function(fileName, sep, header, row.names, col.names,
           type, skip, separated, backingfile, backingpath, 
           descriptorfile, extraCols)
  {
    if ( (header | is.character(col.names)) & is.numeric(extraCols) )
      stop(paste("When column names are specified, extraCols must be the names",
                 "of the extra columns."))
    if (!header & is.null(col.names) & is.character(extraCols))
      stop(paste("No header and no column names specified, so extraCols",
           "must be an integer."))
    headerOffset <- as.numeric(header)
    colNames <- NULL
    if (header) {
      colNames = unlist(strsplit(
        scan(fileName, what='character', skip=skip, nlines=1, sep="\n", 
             quiet=TRUE), split=sep))
      if (is.character(col.names)) {
        warning("Using supplied column names and skipping the header row.\n")
        colNames <- col.names
      } else {
        if (!is.null(col.names))
          stop("You need to decide between column names and a header.\n")
      }
    } else {
      if (is.character(col.names)) colNames <- col.names
    }
    # Get the first line of data
    firstLineVals <- unlist(strsplit(
      scan(fileName, what='character', skip=(skip+headerOffset), 
           nlines=1, sep="\n", quiet=TRUE), split=sep))

    # See if there are row names (a row name always has a double quote)
    rowNames <- NULL
    userowNames <- FALSE
    hasQuoteInFirstLine <- grep( '"', firstLineVals )
    hasRowNames <- (length(hasQuoteInFirstLine) > 0 && hasQuoteInFirstLine > 0)
    if (hasRowNames) userowNames <- TRUE

    if (length(colNames) == length(firstLineVals)) {
      # No row names, may or may not be column names, but we don't care
      # and nothing more needs to be done at this point.
    }
    if (!is.null(colNames) && (length(colNames)==length(firstLineVals))) {
      # Column and row names both exist
      if (is.logical(row.names) && !row.names) 
        stop("Error: row names seem to exist in this file.\n")
      if (is.logical(row.names) && row.names) {
        firstLineVals <- firstLineVals[-1]
        userowNames <- TRUE
      }
      if (is.character(row.names)) {
        warning("Using supplied row names instead of those in the file.\n")
        rowNames <- row.names
        userowNames <- FALSE
      }
    } 
    if (is.null(colNames) && is.logical(row.names)) {
      if (row.names) {
        firstLineVals <- firstLineVals[-1]
        userowNames <- TRUE
      }
    }

    numCols <- length(firstLineVals) - as.integer(hasRowNames)
    if (is.na(type)) {
      warning(paste("big.matrix type was not specified, going by first line,",
                    "noting that a choice will be made between double and",
                    "integer only (not short or char)."))
      type <- 'double'
      if (sum(na.omit(as.integer(firstLineVals)) ==
              na.omit(as.double(firstLineVals))) ==
          numCols ) 
        type <- 'integer'
    }

    lineCount <- .Call("CCountLines", fileName) - skip - headerOffset
    numRows <- lineCount
    createCols <- numCols
    if (is.numeric(extraCols)) createCols = createCols + extraCols
    if (is.character(extraCols)) {
      createCols <- createCols + length(extraCols)
      colNames <- c(colNames, extraCols)
    }
    bigMat <- big.matrix(nrow=numRows, ncol=createCols, type=type,
                         dimnames=list(rowNames, colNames), init=NULL, 
                         separated=separated, backingfile=backingfile,
                         backingpath=backingpath, descriptorfile=descriptorfile)

    ############################################################
    # if userowNames == NULL, then there aren't any in the file.
    # if userowNames == TRUE, then they exist: use them!
    # if userowNames == FALSE, then they exist, but you ignore them.
    .Call('ReadMatrix', fileName, bigMat@address, 
          as.integer(skip+headerOffset), as.integer(numRows), 
          as.integer(numCols), sep, hasRowNames, userowNames)

    return(bigMat)
  })

setGeneric('write.big.matrix', 
  function(x, fileName, row.names=FALSE, col.names=FALSE, sep=",") 
    standardGeneric('write.big.matrix'))

setMethod('write.big.matrix', signature(x='big.matrix',fileName='character'),
  function(x, fileName, row.names, col.names, sep)
  {
    if (is.character(row.names))
      stop("You must set the row names before writing.\n")
    if (is.character(col.names))
      stop("You must set the column names before writing.\n")
    if (row.names & !.Call("HasRowColNames",x@address)[1]) {
      row.names <- FALSE
      warning("No row names exist, overriding your row.names option.\n")
    }
    if (col.names & !.Call("HasRowColNames",x@address)[2]) {
      col.names <- FALSE
      warning("No column names exist, overriding your col.names option.\n")
    }
    .Call('WriteMatrix', x@address, fileName, as.logical(row.names), 
      as.logical(col.names), sep)
    invisible(NULL)
  })

setGeneric('is.separated', function(x) standardGeneric('is.separated'))

setMethod('is.separated', signature(x='big.matrix'),
  function(x)
  {
    return(.Call("IsSeparated", x@address))
  })

cleanupcols <- function(cols=NULL, nc=NULL, colnames=NULL) {
  if (is.null(cols)) cols <- 1:nc
  else {
    if (!is.numeric(cols) & !is.character(cols) & !is.logical(cols))
      stop("column indices must be numeric, logical, or character vectors.")
    if (is.character(cols))
      if (is.null(colnames)) stop("column names do not exist.")
      else cols <- mmap(cols, colnames)
    if (is.logical(cols)) {
      if (length(cols) != nc)
        stop(paste("column vector length must match the number of",
                   "columns of the matrix."))
      cols <- which(cols)
    }
    tempj <- .Call("CCleanIndices", as.double(cols), as.double(nc))
    if (is.null(tempj[[1]])) stop("Illegal column index usage in extraction.\n")
    if (tempj[[1]]) cols <- tempj[[2]]
  }
  return(cols)
}

deepcopy <- function(x, cols=NULL, type=NULL, separated=NULL,
                     backingfile=NULL, backingpath=NULL,
                     descriptorfile=NULL)
{
  cols <- cleanupcols(cols, ncol(x), colnames(x))
  if (nrow(x) > 2^31-1)
    stop(paste("Too many rows to copy at this point in time;",
               "this may be fixed in the future."))
  if (is.null(type)) type <- typeof(x)
  if (is.null(separated)) separated <- is.separated(x)
  y <- big.matrix(nrow=nrow(x), ncol=length(cols), type=type, init=NULL,
      dimnames = dimnames(x), separated=separated,
      backingfile=backingfile, backingpath=backingpath,
      descriptorfile=descriptorfile)
  for (i in 1:length(cols)) y[,i] <- x[,cols[i]]

  return(y)
}

setMethod('apply', signature(X="big.matrix"),
  function(X, MARGIN, FUN, ...) {
    return(bmapply(X, MARGIN, FUN, ...))
  })

bmapply <- function(X, MARGIN, FUN, ...)
{
  if (length(MARGIN)>1) stop("MARGIN > 1 not supported with big.matrix objects.\n")
  FUN <- match.fun(FUN)
  dn.ans <- dimnames(X)[MARGIN]
  if (MARGIN==1) {
    d2 <- nrow(X)
    ans <- vector("list", nrow(X))
    for (i in 1:d2) {
      tmp <- FUN(X[i,], ...)
      if (!is.null(tmp)) ans[[i]] <- tmp
    }
  } else {
    if (MARGIN==2) {
      d2 <- ncol(X)
      ans <- vector("list", ncol(X))
      for (i in 1:d2) {
        tmp <- FUN(X[,i], ...)
        if (!is.null(tmp)) ans[[i]] <- tmp
      }
    } else {
      stop("Only MARGIN equal to 1 or 2 is supported for a big.matrix.\n")
    }
  }
  ans.list <- is.recursive(ans[[1]])
  l.ans <- length(ans[[1]])
  ans.names <- names(ans[[1]])
  if (!ans.list) 
    ans.list <- any(unlist(lapply(ans, length)) != l.ans)
  if (!ans.list && length(ans.names)) {
    all.same <- sapply(ans, function(x) identical(names(x), ans.names))
    if (!all(all.same)) ans.names <- NULL
  }
  if (ans.list) len.a <- d2
  else len.a <- length(ans <- unlist(ans, recursive = FALSE))
  if (len.a == d2) {
    if (length(dn.ans[[1]])) names(ans) <- dn.ans[[1]]
    return(ans)
  }
  if (len.a > 0 && len.a%%d2 == 0) {
    if (is.null(dn.ans)) 
      dn.ans <- vector(mode = "list", length(d2))
    dn.ans <- c(list(ans.names), dn.ans)
    return(array(ans, c(len.a%/%d2, d2), if (!all(sapply(dn.ans, 
                 is.null))) dn.ans))
  }
  return(ans)
}


# Following the R convention we are going to assume Unix directory 
# separators '/' as opposed to the Windows convention '\'.

fix_path = function(path)
{
  if (is.null(path) || path == '')
  {
    path = ''
    return(path)
  }
  else if (substr(path, nchar(path), nchar(path))!='/')
  {
    if (is.na(file.info(path)$isdir))
      stop("The supplied backing path does not exist.")
    path= paste(path, '/', sep='')
  }
  else 
  {
    if ( is.na(file.info( substr(path, 1, nchar(path)-1) )) )
      stop( "The supplied backing path does not exist.")
  }
  return(path)
}

setGeneric('backingpath', function(x)
  standardGeneric('backingpath'))

setMethod('backingpath', signature(x='big.matrix'),
  function(x)
  {
    return(.Call('GetFileBackedPath',x@address))
  })

setGeneric('is.sub.big.matrix', function(x)
	standardGeneric('is.sub.big.matrix'))

setMethod('is.sub.big.matrix', signature(x='big.matrix'),
  function(x) return(.Call('CIsSubMatrix', x@address)) )

# For now a submatrix only goes over a range of columns and a range
# of row.  This could be made more sophiticated but it would probably
# take a lot of work.
setGeneric('sub.big.matrix', function(x, firstRow=1, lastRow=NULL,
  firstCol=1, lastCol=NULL, backingpath='') standardGeneric('sub.big.matrix'))

setMethod('sub.big.matrix', signature(x='big.matrix'),
  function(x, firstRow, lastRow, firstCol, lastCol, backingpath)
  {
    return(sub.big.matrix(describe(x), firstRow, lastRow, firstCol, lastCol, 
      backingpath))
  })

setMethod('sub.big.matrix', signature(x='big.matrix.descriptor'),
  function( x, firstRow, lastRow, firstCol, lastCol, backingpath)
  {
    rowOffset <- firstRow-1
    colOffset <- firstCol-1
    rbm <- attach.resource(x, backingpath)
    if (is.null(lastRow)) lastRow <- nrow(rbm)
    if (is.null(lastCol)) lastCol <- ncol(rbm)
    numCols <- lastCol-firstCol+1
    numRows <- lastRow-firstRow+1
    if (colOffset < 0 || rowOffset < 0 || numCols < 1 || numRows < 1 ||
      colOffset+numCols > ncol(rbm) || rowOffset+numRows > nrow(rbm))
    {
      rm(rbm)
      stop(paste("A sub.big.matrix object could not be created",
                 "with the specified parameters"))
    }
    .Call("SetRowOffsetInfo", rbm@address, 
          as.double(rowOffset + .Call("GetRowOffset", rbm@address)), 
          as.double(numRows) )
    .Call("SetColumnOffsetInfo", rbm@address, 
          as.double(colOffset + .Call("GetColOffset", rbm@address)),
          as.double(numCols))
    return(rbm)
  })

filebacked.big.matrix=function(nrow, ncol, type='integer', init=NULL,
  dimnames=NULL, separated=FALSE, backingfile=NULL, backingpath=NULL, 
  descriptorfile=NULL)
{
  if (nrow < 1 | ncol < 1)
    stop('A big.matrix must have at least one row and one column')
  if (nrow < 1 | ncol < 1)
    stop('A big.matrix must have at least one row and one column')

  typeVal=NULL
  if (type == 'integer') 
    typeVal=4
  if (type == 'double') 
    typeVal=8
  if (type == 'short') 
    typeVal=2
  if (type == 'char') 
    typeVal=1
  if (is.null(typeVal)) stop('invalid type')
  if (!is.null(dimnames)) {
    rownames <- dimnames[[1]]
    colnames <- dimnames[[2]]
  } else {
    rownames <- NULL
    colnames <- NULL
  }
  if (is.null(backingfile))
  {
    stop('You must specify a backing file')
  }
	backingpath = fix_path(backingpath)

  anon.backing = FALSE
  if (backingfile == '')
  {
    backingfile = tempfile()
    backingpath=''
    anon.backing = TRUE
  }

	address = .Call('CCreateFileBackedBigMatrix', as.character(backingfile), 
    as.character(backingpath), as.double(nrow), as.double(ncol), 
    as.character(colnames), as.character(rownames), as.integer(typeVal), 
    as.double(init), as.logical(separated))
  if (is.null(address))
  {
    stop("Error encountered when creating instance of type big.matrix")
  }
  x=new("big.matrix", address=address)
  if (is.null(x))
  {
    stop("Error encountered when creating instance of type big.matrix")
  }
	if (is.null(descriptorfile) && !anon.backing)
	{
		warning(paste("A descriptor file has not been specified.  ",
			"A descriptor named ", backingfile, ".desc will be created.", sep=''))
		descriptorfile = paste( backingfile, ".desc", sep='' )
	}
  if (!anon.backing)
  {
	  dput( describe(x), paste(backingpath, descriptorfile, sep='') )
  }
  return(x)
}

setClass('descriptor', representation(description='list'))

setGeneric('describe', function(x) 
  standardGeneric('describe'))

setGeneric('description', function(x) standardGeneric('description'))

setClass('big.matrix.descriptor', contains='descriptor')

setMethod('describe', signature(x='big.matrix'),
  function(x)
  {
    return(new('big.matrix.descriptor', description=DescribeBigMatrix(x)))
  })

setMethod('description', signature(x='big.matrix.descriptor'),
  function(x) return(x@description))

DescribeBigMatrix = function(x) #, file=NULL, path="")
{
  if (!is.filebacked(x))
  {
    ret = list(sharedType='SharedMemory',
        sharedName=shared.name(x), 
        totalRows = .Call("GetTotalRows", x@address),
        totalCols = .Call("GetTotalColumns", x@address),
        rowOffset = .Call("GetRowOffset", x@address),
        colOffset = .Call("GetColOffset", x@address),
        nrow=nrow(x), ncol=ncol(x),
        rowNames=rownames(x), colNames=colnames(x), type=typeof(x), 
        separated=is.separated(x))
  }
  else
  {
    ret = list(sharedType='FileBacked',
        sharedName=shared.name(x), fileName=file.name(x),
        totalRows = .Call("GetTotalRows", x@address),
        totalCols = .Call("GetTotalColumns", x@address),
        rowOffset = .Call("GetRowOffset", x@address),
        colOffset = .Call("GetColOffset", x@address),
        nrow=nrow(x), ncol=ncol(x),
        rowNames=rownames(x), colNames=colnames(x), type=typeof(x), 
        separated=is.separated(x))
  }
}

setGeneric('attach.resource', 
  function(x, ...) standardGeneric('attach.resource'))

setMethod('attach.resource', signature(x='character'),
  function(x, ...)
  {
    path = match.call()[['path']]
    if (is.null(path))
    {
      path = ''
    }
    info <- dget(paste(fix_path(path), x, sep=""))
    return(attach.resource(info))
  })

setMethod('attach.resource', signature(x='big.matrix.descriptor'),
  function(x, ...)
  {
    path = match.call()[['path']]
    if (is.null(path))
    {
      path = ''
    }
    info = description(x)
    typeLength = NULL
    if (info$type == 'char') typeLength=1
    if (info$type == 'short') typeLength=2
    if (info$type == 'integer') typeLength=4
    if (info$type == 'double') typeLength=8
    if (is.null(typeLength)) 
      stop('invalid type')
    path = fix_path(path)
    if (info$sharedType == 'SharedMemory')
    {
      address=.Call('CAttachSharedBigMatrix', info$sharedName, info$totalRows, 
        info$totalCols, as.character(info$rowNames), 
        as.character(info$colNames), as.integer(typeLength), info$separated)
    }
    else
    {
      address = .Call('CAttachFileBackedBigMatrix', info$sharedName, 
        info$fileName, path, info$totalRows, info$totalCols, 
        as.character(info$rowNames), as.character(info$colNames), 
        as.integer(typeLength), info$separated)
    }
    if (!is.null(address)) 
    {
      .Call("SetRowOffsetInfo", address, info$rowOffset, info$nrow)
      .Call("SetColumnOffsetInfo", address, info$colOffset, info$ncol)
      ans <- new('big.matrix', address=address)
    }
    else 
    {
      stop("Fatal error in attach: big.matrix could not be attached.")
    }
    return(ans)  
  })

attach.big.matrix = function(obj, ...)
{
  attach.resource(obj, ...)
}

setGeneric('is.filebacked', function(x) standardGeneric('is.filebacked'))

setMethod('is.filebacked', signature(x='big.matrix'),
  function(x)
  {
    return(.Call("IsFileBackedBigMatrix", x@address))
  })

setGeneric('shared.name', function(x) standardGeneric('shared.name'))

setMethod('shared.name', signature(x='big.matrix'),
  function(x)
  {
    return(.Call('SharedName', x@address))
  })

setGeneric('file.name', function(x) standardGeneric('file.name'))

setMethod('file.name', signature(x='big.matrix'),
  function(x)
  {
    if (!is.filebacked(x))
    {
      stop("The argument is not a file backed big.matrix.")
    }
    return(.Call('FileName', x@address))
  })

