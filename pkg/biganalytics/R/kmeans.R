# /*
#  *  biganalytics: an R package containing a library of functions for
#  *  use with big.matrix objects of package bigmemory.
#  *
#  *  Copyright (C) 2010 John W. Emerson and Michael J. Kane
#  *
#  *  This file is part of biganalytics.
#  *
#  *  biganalytics is free software; you can redistribute it and/or modify
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

#
# k-means cluster analysis
#
# - Support matrix or big.matrix objects.  WORKING.
# - If nstart>1 generate starting centers up front.  DONE.
# - Use caution checking for duplicates in the starting centers. DONE.
# - Use foreach for sequential or parallel execution with nstart>1.
#	With a matrix:		- sequential doesn't require copy. DONE.
#				- parallel requires a copy. DONE?
#	With a big.matrix:	- SMP is fine               DONE.
#				- Distributed requires filebacking  DONE.
#				  which may require a copy.      DONE?
# - Template support for different types.  WORKING
# - Worker returns only cluster centers and WSS for memory-efficiency.  WORKING
# - End by figuring out cluster memberships rather than storing along the
#   way.  WORKING
# - Each worker uses local vector of indextype (LONG) for cluster memberships.  WORKING
# - We don't use tol
# - We use only MacQueen
# NEW: add flag for SMP versus distributed?

bigkmeans <- function(x, centers, iter.max = 10, nstart = 1) {

  require(foreach)

  ################################################################
  # This function is used to construct a list of length nstart
  # of centers so that there are no duplicates.  If this dies,
  # the user probably shouldn't be using k-means.
  getcenters <- function(x, k, nstart) {
    n <- nrow(x)
    centers <- list(x[sample(1:n, k),,drop=FALSE])
    for (ii in 1:(10+2^k)) {
      if (any(duplicated(centers[[length(centers)]]))) {
        centers[[length(centers)]] <- x[sample(1:n, k),,drop=FALSE]
      } else break;
    }
    if (any(duplicated(centers[[length(centers)]]))) {
      stop("Having trouble finding non-duplicated centers.\n")
    }
    if (nstart>1) {
      for (i in 2:nstart) {
        centers[[length(centers)+1]] <- x[sample(1:n, k),,drop=FALSE]
        for (ii in 1:(10+2^k)) {
          if (any(duplicated(centers[[length(centers)]]))) {
            centers[[length(centers)]] <- x[sample(1:n, k),,drop=FALSE]
          } else break;
        }
        if (any(duplicated(centers[[length(centers)]]))) {
          stop("Having trouble finding non-duplicated centers.\n")
        }
      }
    }
    return(centers)
  }

  ####################################################################
  # A function for aggregating results as foreach is running, to avoid
  # memory overhead.
  choosebest <- function(a, b) {
    if ( sum(a$withinss) < sum(b$withinss) ) {
      return(a)
    } else {
      return(b)
    }
  }
    
  #################################################
  # Check centers for sanity and consider nstart>1:
  if (!is.matrix(centers)) {
    if (is.numeric(centers) && length(centers)==1 && centers>0) {
      k <- centers
      centers <- getcenters(x, k, nstart)
    } else stop("centers must be a matrix of centers or number of clusters > 0")
  } else {
    k <- nrow(centers)
    if (nstart>1) {
      warning(paste("Random starting points will be used",
                    "(not the centers you provided), because nstart>1.\n"))
      centers <- getcenters(x, k, nstart)
    } else {
      if (any(duplicated(centers))) {
        stop(paste("Error: if you provide centers,",
                   "they had better not have duplicates.\n"))
      }
      centers <- list(centers)
    }
  }

  ###############################################################
  # At this point, centers is a list of length nstart of matrices
  # of starting centers without duplicates.
  # I think I allow k=1 cluster, too, but check it later.
  # Note that if number of columns is HUGE, the centers will
  # be memory-intensive.

  if (is.matrix(x)) {
    if (getDoParWorkers()>1) {
      # Generate big.matrix copy so we can work in parallel; we
      # assume in-memory is fine given that x is a matrix.
      if (is.integer(x)) {
        y <- as.big.matrix(x, type="integer")
      } else {
        y <- as.big.matrix(x, type="double")
      }
      xdesc <- describe(y)
    } else { xdesc <- NULL }       # This is the signal of a matrix.
  } else {
    if (is.big.matrix(x)) {
      xdesc <- describe(x)
    } else {
      stop("x must be a matrix or a big.matrix.\n")
    }
  }

  nr <- nrow(x)
  if (typeof(x)=="char") mattype <- 1
  if (typeof(x)=="short") mattype <- 2
  if (typeof(x)=="integer") mattype <- 4
  if (typeof(x)=="double") mattype <- 8

  cen <- NA # Note this is because of foreach's interaction with the
            # R check parser.  There is no real problem, but I need this
            # to avoid a warning.

  # Do the work, possibly in parallel with nstart>1 and a registered
  # parallel backend.
  ans <- foreach(cen=centers, .combine="choosebest") %dopar% {

    # Note that at this point, we're on a worker; I use a local big.matrix
    # object for centers to make the C++ code easier [][] matrix notation.

    require(biganalytics)
    center <- big.matrix(nrow(cen), ncol(cen), type="double")
    center[,] <- cen
    clust <- big.matrix(nr, 1, type="integer")
    clustsizes <- big.matrix(nrow(cen), 1, type="double")
    wss <- big.matrix(nrow(cen), 1, type="double")
    if (is.null(xdesc)) {
      # .Call with the matrix, which has to be either integer or double.
      if (mattype==4) {
        res <- .Call("kmeansRIntMatrix", x,
                     center@address, clust@address, clustsizes@address,
                     wss@address, as.integer(iter.max))
      } else {
        res <- .Call("kmeansRNumericMatrix", x,
                     center@address, clust@address, clustsizes@address,
                     wss@address, as.integer(iter.max))
      }
    } else {
      # .Call with the big.matrix
      x <- attach.big.matrix(xdesc)
      res <- .Call("kmeansBigMatrix", x@address,
                   center@address, clust@address, clustsizes@address,
                   wss@address, as.integer(iter.max))
    }

    temp <- list(cluster=clust[,],
                 centers=center[,],
                 withinss=wss[,],
                 size=clustsizes[,],
                 iters=res)

    return(temp)

  } # End of the foreach() body.

  if (ans$iters>=iter.max) 
    warning("bigkmeans did not converge in ", iter.max, " iterations.\n")
  ans$iters <- NULL                     # This removes this from the list.
  class(ans) <- "kmeans"

  return(ans)

}


