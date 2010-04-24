library(bigmemory)
options(bigmemory.allow.dimnames=TRUE)

TestGet=function(x, numRanTests=30)
{
  y = x[,]
  if (any(x[,] != y))
    stop("problem with y = x[,]")
  for (i in 1:numRanTests)
  {
    ranRows = sample(nrow(x), sample(nrow(x), 1)-1)
    ranCols = sample(ncol(x), sample(ncol(x), 1)-1)

    # Positive index testing.
    if ( any(x[ranRows,] != y[ranRows,]) )
      stop("problem with x[ranRows,] != y[ranRows,]")
    if ( any(x[,ranCols] != y[,ranCols]) )
      stop("problem with x[,ranCols] != y[,ranCols]")

    # Negative index testing  
    if ( any(x[-ranRows,-ranCols] != y[-ranRows,-ranCols]) )
      stop("problem with x[-ranRows,-ranCols] != y[-ranRows,-ranCols]")
    if ( any(x[-ranRows,] != y[-ranRows,]) )
      stop("problem with x[-ranRows,] != y[-ranRows,]")
    if ( any(x[,-ranCols] != y[,-ranCols]) )
      stop("problem with x[,-ranCols] != y[,-ranCols]")

    # Logical testing
    i <- rep(FALSE, nrow(x))
    i[ranRows] <- TRUE
    j <- rep(FALSE, ncol(x))
    j[ranCols] <- TRUE
    if (any(x[i,j] != y[i,j])) stop("Problem with logical 1")
    if (any(x[i,] != y[i,])) stop("Problem with logical 2")
    if (any(x[,j] != y[,j])) stop("Problem with logical 3")

    # Character testing
    if (!is.null(colnames(x)) & !is.null(rownames(x))) {
      i <- rownames(x)[ranRows]
      j <- colnames(x)[ranCols]
      if (any(x[i,j] != y[i,j])) stop("Problem with character 1")
      if (any(x[i,] != y[i,])) stop("Problem with character 2")
      if (any(x[,j] != y[,j])) stop("Problem with character 3")
    }

  }
}

TestSet <- function(x, numRanTests=30) {
  y <- x
  x[,] <- 0; y[,] <- 0
  if (any(x[,] != y[,])) stop("problem main set 1")
  x[,] <- rep(1, nrow(x)); y[,] <- rep(1,nrow(x))
  if (any(x[,] != y[,])) stop("problem main set 2")
  x[,] <- matrix(1:(nrow(x)*ncol(x)), nrow(x), ncol(x))
  y[,] <- matrix(1:(nrow(x)*ncol(x)), nrow(x), ncol(x))
  if (any(x[,] != y[,])) stop("problem main set 3")

  for (i in 1:numRanTests)
  {
    print(i)
    ranRows = sample(nrow(x), sample(nrow(x), 1)-1)
    ranCols = sample(ncol(x), sample(ncol(x), 1)-1)

    # Lots of testing
    for (kk in 1:3) {
      if (kk==1) {
        i <- rep(FALSE, nrow(x))
        i[ranRows] <- TRUE
        j <- rep(FALSE, ncol(x))
        j[ranCols] <- TRUE
      }
      if (kk==2) {
        i <- ranRows
        j <- ranCols
      }
      if (kk==3) {
        if (!is.null(colnames(x)) & !is.null(rownames(x))) {
          i <- rownames(x)[ranRows]
          j <- colnames(x)[ranCols]
        }
      }
      x[i,j] <- 0; y[i,j] <- 0
      if (any(x[,] != y[,])) stop(paste("Problem with set 1",kk))
      x[,j] <- 1; y[,j] <- 1
      if (any(x[,] != y[,])) stop(paste("Problem with set 2",kk))
      x[i,] <- 3; y[i,] <- 3
      if (any(x[,] != y[,])) stop(paste("Problem with set 3",kk))
      if (kk!=1) {
        x[i,j] <- rep(0,length(i)); y[i,j] <- rep(0,length(i))
        if (any(x[,] != y[,])) stop(paste("Problem with set 4",kk))
        x[,j] <- rep(1,nrow(x)); y[,j] <- rep(1,nrow(x))
        if (any(x[,] != y[,])) stop(paste("Problem with set 5",kk))
        x[i,] <- rep(2,length(i)); y[i,] <- rep(2,length(i))
        if (any(x[,] != y[,])) stop(paste("Problem with set 6",kk))
      }
      # Negative if numeric only:
      if (kk==2) {
        x[-i,-j] <- 0; y[-i,-j] <- 0
        if (any(x[,] != y[,])) stop(paste("Problem with set 1",kk))
        x[,-j] <- 1; y[,-j] <- 1
        if (any(x[,] != y[,])) stop(paste("Problem with set 2",kk))
        x[-i,] <- 2; y[-i,] <- 2
        if (any(x[,] != y[,])) stop(paste("Problem with set 3",kk))
      }
    }
  }
}

TestIndices=function( x, numRanTests=30 )
{
  y = x[,]
  for (i in 1:nrow(x))
  {
    if ( sum(as.vector(x[-i,] != y[-i,]), na.rm=TRUE) != 0)
    {
      cat("problem in row ", i, "\n")
    }
  }
  for (j in 1:ncol(x))
  {
    if ( sum(as.vector(x[,-j] != y[,-j]), na.rm=TRUE) != 0)
    {
      cat("problem in col ", j, "\n")
    }
    if ( min(y[,j], na.rm=TRUE) != min(x[,j], na.rm=TRUE) )
    {
      cat("problem in colmin ", j, "\n")
    }
    if ( max(y[,j], na.rm=TRUE) != max(x[,j], na.rm=TRUE) )
    {
      cat("problem in colmax ", j, "\n")
    }
#   if ( min(y[,j], na.rm=TRUE) != colmin(x, j, na.rm=TRUE) )
#   {
#     cat("problem in colmin ", j, "\n")
#   }
#   if ( max(y[,j], na.rm=TRUE) != colmax(x, j, na.rm=TRUE) )
#    {
#     cat("problem in colmax ", j, "\n")
#    }
  }
  for (i in 1:numRanTests)
  {
    numRanCols = sample(ncol(x), 1)
    numRanRows= sample(ncol(x), 1)
    cols = sample(ncol(x), numRanCols)
    rows = sample(nrow(x), numRanRows)

    if ( sum(as.vector(x[,-cols] != y[,-cols]), na.rm=TRUE) != 0)
    {
      cat("problem in cols ", cols, "\n")
    }
    if ( sum(as.vector(x[-rows,] != y[-rows,]), na.rm=TRUE) != 0)
    {
      cat("problem in rows ", rows, "\n")
    }
    if ( sum(as.vector(x[-rows,-cols] != y[-rows,-cols]), na.rm=TRUE) != 0)
    {
      cat("problem in -rows , -cols", rows, cols, "\n")
    }
    if ( sum(as.vector(x[rows,-cols] != y[rows,-cols]), na.rm=TRUE) != 0)
    {
      cat("problem in rows , -cols", rows, cols, "\n")
    }
    if ( sum(as.vector(x[rows,-cols] != y[rows,-cols]), na.rm=TRUE) != 0)
    {
      cat("problem in rows , -cols", rows, cols, "\n")
    }
    if ( sum(as.vector(x[rows,cols] != y[rows,cols]), na.rm=TRUE) != 0)
    {
      cat("problem in rows , cols", rows, cols, "\n")
    }
  }
  for (i in 1:numRanTests)
  {
    ranCols = rep(FALSE, ncol(x))
    ranCols[sample( ncol(x), sample(1:ncol(x)))] = TRUE
    ranRows = rep(FALSE, nrow(x))
    ranRows[sample( nrow(x), sample(1:nrow(x)))] = TRUE
    if ( sum(as.vector(x[rows,cols] != y[rows,cols]), na.rm=TRUE) != 0)
    {
      cat("problem in rows , cols", rows, cols, "\n")
    }
  }
}

numCols = 30
numRows = 100
#types = c("char", "short", "integer", "double")
types = c("short", "integer", "double")
sharedParams = c(FALSE, TRUE, TRUE)
rownames <- paste("X", as.character(1:numRows), sep="")
colnames <- paste("Y", as.character(1:numCols), sep="")
options(bigmemory.typecast.warning=FALSE)

for (i in 1:length(sharedParams))
{
  for (type in types)
  {
    x = matrix( 1:(numCols*numRows), nrow=numRows, ncol=numCols,
                dimnames=list(rownames,colnames) )
    shared = sharedParams[i]
    if (i == length(sharedParams)) {
      backingfile = 'test.bin'
    } else {
      backingfile = NULL
    }
    x = as.big.matrix( x, type=type, backingfile=backingfile)
    cat("Starting the TestGet run.\n")
    TestGet(x)
    cat("Starting the TestSet run.\n")
    TestSet(x)
    cat("Starting the TestIndices run.\n")
    TestIndices(x)
  }
  cat("Test", i, "completed\n")
}

