library(bigmemory)
library(biganalytics)
options(bigmemory.allow.dimnames=TRUE)

x <- big.matrix(5, 3, type="double")
colnames(x) <- c("A", "B", "C")
rownames(x) <- c("a", "b", "c", "d", "e")
x[,] <- sample(1:3, 15, replace=TRUE)
y <- x[,]
if (sum(apply(x, 1, range) != apply(y, 1, range)) > 0)
  stop("Problem with apply")

if (sum(apply(x, 2, range) != apply(y, 2, range)) > 0)
  stop("Problem with apply")

if (sum(apply(x, 1, min) != apply(y, 1, min)) > 0)
  stop("Problem with apply")

if (sum(apply(x, 2, min) != apply(y, 2, min)) > 0)
  stop("Problem with apply")

if (sum(unlist(apply(x, 1, unique)) != unlist(apply(y, 1, unique))) > 0)
  stop("Problem with apply")

if (sum(unlist(apply(x, 2, unique)) != unlist(apply(y, 2, unique))) > 0)
  stop("Problem with apply")

