# b = video()
#x = cbind(as.vector(row(b)), as.vector(col(b)), as.vector(b))

video = function() {
  x = new('big.matrix',address=.Call("GrabVideo", as.integer(10)))
  b = matrix(x[,1], 640, 480 )
  y = cbind(as.vector(row(b)), as.vector(col(b)), as.vector(b))
  return(y)
}
