library(testthat)
library(stringr)
library(bigmemory)

context("deepcopy")

# input for different tests
create_big_matrix <- function(xdim=20, ydim=10, ...) {
    as.big.matrix(matrix(rnorm(xdim*ydim), xdim, ydim), ...)
}
create_matrix <- function(xdim=20, ydim=10) {
  matrix(rnorm(xdim*ydim), xdim, ydim)
}


# 1.
test_that("works with R-style matrix as x", {
  x <- create_matrix()
  y <- deepcopy(x)
  expect_that(x, is_equivalent_to(y[,]))
})

# 2.
test_that("works with big matrix as x", {
  x <- create_big_matrix()
  y <- deepcopy(x)
  expect_that(x[,], equals(y[,]))
})

# 3a.
test_that("works with subset of rows/columns using big matrix", {
  x <- create_big_matrix(xdim=30, ydim=20)
  y <- deepcopy(x, rows=10:25, cols=5:15)
  expect_that(x[10:25,5:15], equals(y[,]))
})

# 3b.
test_that("works with subset of rows using big matrix", {
  x <- create_big_matrix(xdim=30, ydim=20)
  y <- deepcopy(x, rows=10:25)
  expect_that(x[10:25,], equals(y[,]))
})

# 3c.
test_that("works with subset of columns using big matrix", {
  x <- create_big_matrix(xdim=30, ydim=20)
  y <- deepcopy(x, cols=5:15)
  expect_that(x[,5:15], equals(y[,]))
})

# 4.
test_that("works when y (the output) is given", {
  x <- create_big_matrix(xdim=20, ydim=10, type="double")
  y <- big.matrix(nrow(x), ncol(x), type="double")
  deepcopy(x, y=y)
  expect_that(x[,], equals(y[,]))
})

# 5.
test_that("get error when y (the output) is given and has incorrect dims", {
  # rows don't match - 1
  x <- create_big_matrix(xdim=20, ydim=10, type="double")
  y <- big.matrix(25, ncol(x), type="double")
  expect_that(deepcopy(x, y=y), throws_error(
    "length of row indices does not equal # of rows in new matrix"))

  # rows don't match - 2
  x <- create_big_matrix(xdim=20, ydim=10, type="double")
  y <- big.matrix(nrow(x), ncol(x), type="double")
  expect_that(deepcopy(x, rows=1:10, y=y), throws_error(
    "length of row indices does not equal # of rows in new matrix"))
 
  # cols don't match - 1
  x <- create_big_matrix(xdim=20, ydim=10, type="double")
  y <- big.matrix(nrow(x), 15, type="double")
  expect_that(deepcopy(x, y=y), throws_error(
    "length of col indices does not equal # of cols in new matrix"))
  
  # cols don't match - 2
  x <- create_big_matrix(xdim=20, ydim=10, type="double")
  y <- big.matrix(nrow(x), ncol(x), type="double")
  expect_that(deepcopy(x, cols=4:8, y=y), throws_error(
    "length of col indices does not equal # of cols in new matrix"))
})

# 6. works when x and y are of different data types
test_that("works when x and y are different data types (y not given)", {
  oldbtw <- getOption("bigmemory.typecast.warning")
  options(bigmemory.typecast.warning=FALSE)
  
  # double -> integer
  x <- big.matrix(20, 10, init=10, type="double")
  y <- deepcopy(x, type="integer")
  expect_that(x[,], equals(y[,]))
  
  # integer -> short
  x <- big.matrix(20, 10, init=10, type="integer")
  y <- deepcopy(x, type="short")
  expect_that(x[,], equals(y[,]))
  
  # short -> char
  x <- big.matrix(20, 10, init=10, type="short")
  y <- deepcopy(x, type="char")
  expect_that(x[,], equals(y[,]))
  
  options(bigmemory.typecast.warning=oldbtw)
})

test_that("give warning when x and y are different data types (y not given)", {
  oldbtw <- getOption("bigmemory.typecast.warning")
  options(bigmemory.typecast.warning=TRUE)
  
  # double -> integer
  x <- big.matrix(20, 10, init=10, type="double")
  expect_that(deepcopy(x, type="integer"), gives_warning())

  # integer -> short
  x <- create_big_matrix(type="integer")
  expect_that(deepcopy(x, type="short"), gives_warning())

  # short -> char
  x <- create_big_matrix(type="short")
  expect_that(deepcopy(x, type="char"), gives_warning())
    
  options(bigmemory.typecast.warning=oldbtw)
})

# 7. works when x and y are of different separated types
test_that("works when x and y are different separated types", {
  # x = yes, y = no
  x <- create_big_matrix(separated=TRUE)
  y <- deepcopy(x, separated=FALSE)
  expect_that(x[,], equals(y[,]))
  
  # x = no, y = yes
  x <- create_big_matrix(separated=FALSE)
  y <- deepcopy(x, separated=TRUE)
  expect_that(x[,], equals(y[,]))
})
