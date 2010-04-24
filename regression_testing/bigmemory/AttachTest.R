library(bigmemory)

x = as.big.matrix( as.matrix(iris), type='double', backingfile='myback.bin',
  backingpath='dir1', descriptorfile="myback.desc")
y = attach.big.matrix( dget('dir1/myback.desc'), path="dir1" )
y = attach.big.matrix( 'dir1/myback.desc' )


