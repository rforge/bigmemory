probecamera <- function() {

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

probemovie <- function(filename) {
  stop("Not yet implemented.")
}

testcamera <- function(x, color=TRUE, frames=100) {

  # OPTIONS include grayscale vs color, blurring, etc...
  # Check out available opencv options that might be nice.
  # The default is probably 7-8 seconds of camera time.

  ans <- .C("Ctestcamera", color=as.integer(color), T=as.integer(frames))
  if (ans$color==-1) stop("unable to find your camera, sorry!")

}

videobuffer <- function(frames=100, color=FALSE,
                        structure=c("big.matrix", "matrix"),
                        init=NULL, type=NULL, dimnames=NULL,
                        separated=FALSE,
                        backingfile=NULL, descriptorfile=NULL,
                        shared=TRUE, moviefile=NULL) {

  if (length(structure)>1) structure <- structure[1]
  if (color) stop("color buffer not yet implemented.")
  if (frames<1) stop("frames must be a positive integer")

  if (!is.null(moviefile)) {
    v <- probemovie(moviefile)
  } else {
    v <- probecamera()
  }
  if (structure=="big.matrix") {
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
  }
  ans <- list(data=x, structure=structure)
  ans$type <- bigmemory::typeof(x)
  ans$width <- v$Width[1+color]
  ans$height <- v$Height[1+color]
  ans$frames <- frames
  ans$color <- color

  class(ans) <- "videobuffer"
  return(ans)
}

read.movie <- function(moviefile, ...) {
  vb <- videobuffer(..., moviefile=moviefile)
}

record.video <- function(vb=NULL, frames=100, frame.start=1) {

  # record frames of video starting at frame.start
  # loop in C as we've already done.
  # This may have side effects, in which case there is
  # no return; or it may return a new video buffer.

  if (is.null(vb)) {
    doret <- TRUE
    vb <- videobuffer(frames=frames)
    warning(paste("No video buffer (vb) was provided, so using\n",
                  "\tthe default from a call to videobuffer()."))
  }
  if (vb$structure=="matrix") {
    stop("not yet implemented.")
  } else {
    ans <- .Call("CPPrecordvideoBigMatrix", (vb$data)@address,
                 as.integer(vb$width), as.integer(vb$height),
                 as.integer(frames), as.integer(frame.start),
                 as.logical(vb$color))
    if (!ans) stop("Error in recording.\n")
  }

  if (doret) {
    warning(paste("Returning a videobuffer: make sure you did an",
                  "assignment if you care.\n"))
    return(vb)
  }
}

print.videobuffer <- function(vb) {
  cat("Object of class 'videobuffer':\n")
  cat("\tdata:\n")
  cat("\t\tmin  =", min(vb$data[,], na.rm=TRUE), "\n")
  cat("\t\tmax  =", max(vb$data[,], na.rm=TRUE), "\n")
  cat("\t\tmean =", mean(vb$data[,], na.rm=TRUE), "\n")
  cat("\t\tsd =", sd(vb$data[,], na.rm=TRUE), "\n")
  cat("\tstructure:", vb$structure, "\n")
  cat("\ttype:", vb$type, "\n")
  cat("\twidth:", vb$width, "\n")
  cat("\theight:", vb$height, "\n")
  cat("\tframes:", vb$frames, "\n")
  cat("\tcolor:", vb$color, "\n")
}

plotframe <- function(vb, frame=1) {
  a <- matrix(vb$data[,frame], vb$width, vb$height)
  a <- cbind(as.vector(row(a)), as.vector(col(a)), as.vector(a))
  a <- a[!is.na(a[,3]),]
  plot(a[,1], a[nrow(a):1,2], col=gray(a[,3]/256), pch=".",
       xlab="Left/Right", ylab="Up/Down", 
       main="Interface 2010\n(640x480, one frame, grayscale)")
}

plotpixels <- function(vb, pixels=1) {

}

get.frame <- function(vb, frame) {
  # Fill the specified frame column of the buffer
  # This has side effects!

  # Caution: remember the wait somethingorother that is
  # necessary in C with opencv.
}



