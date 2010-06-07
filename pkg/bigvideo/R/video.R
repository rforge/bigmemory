probecamera <- function() {

  # Call C function to figure out the attributes of
  # the available camera, if any.  Trap errors.

  ans <- NA
  class(ans) <- "videofeed"
  return(ans)
}

#initmovie <- function(OPTIONS) {  # or probeAVI?
  #create an object of class "videofeed" that is sufficient
  #to describe the movie you want to read in.
#}

testcamera <- function(x, OPTIONS) {
  # Display just one frame using opencv graphics device.
  # OPTIONS include grayscale vs color, blurring, etc...
  # Check out available opencv options that might be nice.
}

videobuffer <- function(v, frames=1,
                        structure=c("big.matrix", "matrix"),
                        ...) {
  # v is class videofeed
  # structure could also be "matrix"
  # if structure=="big.matrix" then use ... to pass through
  # info on the type of big.matrix

  ans <- list(videofeed=v, x)
  class(ans) <- "videobuffer"
  return(ans)
}

read.video(vb) {
  # use this if vb is a movie file?
  # Or take a filename and return a videobuffer?
}

record.video(vb=NULL, max.frames=1, OPTIONS) {
  # record at most max.frames of video
  # loop in C as we've already done.
  # This may have side effects, in which case there is
  # no return; or it may return a new video buffer.
}

get.frame(vb, frame) {
  # Fill the specified frame column of the buffer
  # This has side effects!

  # Caution: remember the wait somethingorother that is
  # necessary in C with opencv.
}




video = function(T=10) {
  x = new('big.matrix',address=.Call("GrabVideo", as.integer(T)))
  return(x)
}

getframe <- function(x, t=1) {
  b <- matrix(x[,1], 640, 480)
  y = cbind(as.vector(row(b)), as.vector(col(b)), as.vector(b))
  return(y)
}

plotframe <- function(a) {
  a <- a[!is.na(a[,3]),]
  plot(a[,1], a[nrow(a):1,2], col=gray((127+a[,3])/256), pch=".",
       xlab="Left/Right", ylab="Up/Down", 
       main="Interface 2010\n(640x480, one frame, grayscale)")
}
