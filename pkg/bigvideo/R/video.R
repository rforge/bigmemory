probe.camera <- function() {

  # First is bw, second is color.
  # Other useful things?

  ans <- .C("Cprobecamera", Width=rep(as.integer(0), 2),
                            Height=rep(as.integer(0), 2),
                            elemSize=rep(as.integer(0), 2),
                            channelSize=rep(as.integer(0), 2))

  if (ans$Width[1]==-1) stop("unable to find your camera, sorry!")

  class(ans) <- "videofeed"
  return(ans)
}

probe.movie <- function(filename) {
  stop("Not yet implemented.")
}

test.camera <- function(x, color=TRUE, frames=100) {

  # OPTIONS include grayscale vs color, blurring, etc...
  # Check out available opencv options that might be nice.
  # The default is probably 7-8 seconds of camera time.

  ans <- .C("Ctestcamera", color=as.integer(color), T=as.integer(frames))
  if (ans$color==-1) stop("unable to find your camera, sorry!")

}

videobuffer <- function(frames=100, color=FALSE, delay=20,
                        structure=c("big.matrix", "matrix"),
                        init=NULL, type=NULL, dimnames=NULL,
                        separated=FALSE,
                        backingfile=NULL, descriptorfile=NULL,
                        shared=TRUE, moviefile=NULL) {

  if (length(structure)>1) structure <- structure[1]
  if (color) stop("color buffer not yet implemented.")
  if (frames<1) stop("frames must be a positive integer")

  if (!is.null(moviefile)) {
    v <- probe.movie(moviefile)
  } else {
    v <- probe.camera()
  }
  if (structure=="big.matrix") {
    suppressPackageStartupMessages(hasbigmem <- require(bigmemory))
    if (!hasbigmem) stop("package bigmemory is required for a big.matrix")
    require(biganalytics)
    if (is.null(type)) {
      if (v$elemSize[1+color]/v$channelSize[1+color] <= 2) type <- "short"
      if (v$elemSize[1+color]/v$channelSize[1+color] > 2) type <- "integer"
      if (v$elemSize[1+color]/v$channelSize[1+color] > 4) type <- "double"
    }
    x <- big.matrix(v$Width[1+color]*v$Height[1+color],
                    as.integer(frames), init=init, type=type,
                    dimnames=dimnames, separated=separated,
                    backingfile=backingfile, descriptorfile=descriptorfile,
                    shared=shared)
    ans <- list(data=x, structure=structure)
    ans$type <- bigmemory::typeof(x)
  } else {
    if (structure=="matrix") {
      if (is.null(init)) {
        init <- as.integer(NA)
      }
      if (!is.null(type)) {
        if (type=="double") init <- as.double(init)
        if (type=="integer") init <- as.integer(init)
      }
      x <- matrix(init, nrow=v$Width[1+color]*v$Height[1+color],
                  ncol=as.integer(frames), dimnames=dimnames)
    } else {
      stop("structure must be 'matrix' or 'big.matrix'.")
    }
    ans <- list(data=x, structure=structure)
    ans$type <- typeof(x)
  }
  ans$width <- v$Width[1+color]
  ans$height <- v$Height[1+color]
  ans$frames <- frames
  ans$color <- color
  ans$delay <- delay
  ans$lastrecorded <- NA
  class(ans) <- "videobuffer"
  return(ans)
}

read.movie <- function(moviefile, ...) {
  vb <- videobuffer(..., moviefile=moviefile)
}

record.video <- function(vb=NULL, frames=NULL, frame.start=1, loop=FALSE,
                         delay=NULL) {

  # record frames of video starting at frame.start.
  # This will have side effects (modifying the specified
  # videobuffer), in which case there is no return;
  # or it may return a new video buffer if none was provided.
  # delay is in milliseconds.

  if (loop) cat("Click in the video window and press any key to stop.\n")
  if (is.null(vb)) {
    doret <- TRUE
    frames <- 100
    if (is.null(delay)) delay <- 20
    vb <- videobuffer(frames=frames, delay=delay)
    warning(paste("No video buffer (vb) was provided, so using\n",
                  "\tthe default from a call to videobuffer()."))
  } else {
    doret <- FALSE
    if (is.null(frames)) frames <- vb$frames
  }
  if (is.null(delay)) delay <- vb$delay
  if (vb$structure=="matrix") {
    stop("not yet implemented.")
  } else {
    ans <- .Call("CPPrecordvideoBigMatrix", (vb$data)@address,
                 as.integer(vb$width), as.integer(vb$height),
                 as.integer(frames), as.integer(frame.start),
                 as.logical(vb$color), as.logical(loop), as.integer(delay))
    if (is.na(ans)) stop("Error in recording.\n")
  }

  if (doret) {
    warning(paste("Returning a videobuffer: make sure you did an",
                  "assignment if you care.\n"))
    vb$lastrecorded <- ans
    return(vb)
  }

  return(ans)

}

shift.buffer <- function(vb) {
  if (is.na(vb$lastrecorded)) stop("lastrecorded must be set to do shift.")
  
  stop("Not yet implemented.")

}

print.videobuffer <- function(vb) {
  cat("Object of class 'videobuffer':\n")
  cat("\tdata:\n")
  if (vb$structure=="big.matrix") {
    cat("\t\tmin  =", min(colmin(vb$data, na.rm=TRUE)), "\n")
    cat("\t\tmax  =", max(colmax(vb$data, na.rm=TRUE)), "\n")
    cat("\t\tmean =", mean(colmean(vb$data, na.rm=TRUE)), "\n")
  } else {
    cat("\t\tmin  =", min(vb$data, na.rm=TRUE), "\n")
    cat("\t\tmax  =", max(vb$data, na.rm=TRUE), "\n")
    cat("\t\tmean =", mean(vb$data, na.rm=TRUE), "\n")
  }
  cat("\tstructure:", vb$structure, "\n")
  cat("\ttype:", vb$type, "\n")
  cat("\twidth:", vb$width, "\n")
  cat("\theight:", vb$height, "\n")
  cat("\tframes:", vb$frames, "\n")
  cat("\tdelay:", vb$delay, "\n")
  cat("\tcolor:", vb$color, "\n")
}

plot.videobuffer <- function(vb, ...) {
  plotframe(vb, 1, ...)
}

plotframe <- function(vb, frame=1, ...) {
  a <- matrix(vb$data[,frame], vb$width, vb$height)
  a <- cbind(as.vector(row(a)), as.vector(col(a)), as.vector(a))
  a <- a[!is.na(a[,3]),]
  plot(a[,1], a[nrow(a):1,2], col=gray(a[,3]/256), pch=".", ...)
}

plotpixels <- function(vb, pixels=1, ...) {
  #... should be options to lines only.
  x <- 1:vb$frames
  ylim <- range(vb$data[pixels,], na.rm=TRUE)
  xlim <- c(-20, vb$frames)
  par(mar=c(1,1,1,1))
  plot(x, vb$data[pixels[1],], pch="",
       xaxt="n", yaxt="n", xlab="", ylab="", ylim=ylim, xlim=xlim)
  for (i in pixels) {
    lines(x, vb$data[i,], ...)
    text(-10, vb$data[i,1], i)
  }
}

#init.camera <- function(vb, initframes=100) {
#
#  ans <- .Call("CPPinitcamera", as.logical(vb$color), as.integer(initframes))
#  if (!ans) stop("Error in initialization.\n")
#
#}

#get.frame <- function(vb, frames=1000) {
#
#  # Hmm... need to keep cap as an external ptr I think?
#
#  init.camera(vb)
#  for (i in 1:frames) {
#    ans <- .Call("CPPinitcamera", as.logical(vb$color), as.integer(1))
#    if (!ans) stop("Error in initialization.\n")
#  }
#
#}

# Note to Jay: when ... is used, look up how to see if something
# was provided.


