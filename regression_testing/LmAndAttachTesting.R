library(bigmemory)
library(biganalytics)
options(bigmemory.allow.dimnames=TRUE)

# Redo this for each big.matrix type
data(iris)
lm.0 = lm( Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width,
	data=iris)
slm.0 = summary(lm.0)

x = as.big.matrix( as.matrix(iris), type='double' )
biglm.0 = biglm.big.matrix( Sepal.Length ~ Sepal.Width + Petal.Length + 
	Petal.Width, data = x)
sbiglm.0 = summary(biglm.0)

if (sum(abs(slm.0$coefficients[,1]) - abs(sbiglm.0$mat[,1])) > 1e-10)
	print("biglm problem")


x = as.big.matrix( as.matrix(iris), type='double')
rownames(x) = as.character(1:nrow(x))
write.big.matrix(x, 'IrisData.txt', col.names=TRUE, row.names=TRUE)

y = read.big.matrix("IrisData.txt", header=TRUE, has.row.names=TRUE,
	extraCols=c('NewCol1','NewCol2'))
head(y)
y = big.matrix( 3, 3, init=0)
write.big.matrix( y, 'test.txt')
z =read.big.matrix('test.txt', extraCols=3)

write.big.matrix(x,'IrisData2.txt', col.names=TRUE, row.names=FALSE)
x = read.big.matrix('IrisData2.txt', header=TRUE)
head(x)

write.big.matrix(x,'IrisData3.txt', col.names=FALSE, row.names=FALSE)
x = read.big.matrix('IrisData3.txt')
head(x)

z = as.big.matrix( as.matrix(iris), type='double', backingfile='myback.bin',
	backingpath='dir1/', descriptorfile="myback.desc")
x = as.big.matrix( as.matrix(iris), type='double', backingfile='myback.bin',
	backingpath='dir1', descriptorfile="myback.desc")
y = attach.big.matrix( dget('dir1/myback.desc'), "dir1" )
x = as.big.matrix( as.matrix(iris), type='double', backingfile='myback.bin',
	backingpath='dir1', descriptorfile="myback.desc")
y = sub.big.matrix(describe(x), firstRow=2, lastRow=5, firstCol=1, lastCol=3)

