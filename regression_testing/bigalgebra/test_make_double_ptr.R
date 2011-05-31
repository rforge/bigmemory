library(testthat)
library(stringr)
library(bigalgebra)

# tests for the make_double_ptr function in ptr_util.hpp
context("Indirectly checking output of double pointer from big sub matrices")

# input for different tests
create_big_matrix <- function(xdim=20, ydim=10) {
    as.big.matrix(matrix(rnorm(xdim*ydim), xdim, ydim), type="double")
}

# 1. test with dcopy
test_that("dcopy - get error if copying sub.big.matrix with row offset", {
    x <- create_big_matrix(xdim=30, ydim=20)
    message <- str_c("sub.big.matrix objects cannoth have row offset ",
        "greater than zero and number of columns greater than 1")
    
    # subset of rows
    sub_x <- sub.big.matrix(x, firstRow=10, lastRow=20)
    y <- big.matrix(nrow(sub_x), ncol(sub_x), type="double")
    expect_that(dcopy(X=sub_x, Y=y), throws_error(message))
        
    # subset of both rows and columns
    sub_x <- sub.big.matrix(x, firstRow=10, lastRow=20, firstCol=5, lastCol=15)
    y <- big.matrix(nrow(sub_x), ncol(sub_x), type="double")
    expect_that(dcopy(X=sub_x, Y=y), throws_error(message))
})

test_that("dcopy - copying sub big matrices with column offset works", {
    x <- create_big_matrix(xdim=30, ydim=20)
    
    # subset of columns
    sub_x <- sub.big.matrix(x, firstCol=5, lastCol=15)
    y <- big.matrix(nrow(sub_x), ncol(sub_x), type="double")
    dcopy(X=sub_x, Y=y)
    expect_that(sub_x[,], equals(y[,]))
})


# 2. test with dgemm

test_that("dgemm works with big column sub matrices", {
    # when A is a sub.big.matrix
    A <- create_big_matrix(xdim=20, ydim=10)
    sub_A <- sub.big.matrix(A, firstCol=5, lastCol=9)
    B <- create_big_matrix(xdim=5, ydim=20)
    C <- big.matrix(nrow(sub_A), ncol(B), type="double")
    dgemm(A=sub_A, B=B, C=C)
    expect_that((sub_A[,] %*% B[,]), is_equivalent_to(C[,]))
    
    # when B is a sub.big.matrix (and A is a sub.big.matrix)
    sub_B <- sub.big.matrix(B, firstCol=12, lastCol=18)
    C <- big.matrix(nrow(sub_A), ncol(sub_B), type="double")
    dgemm(A=sub_A, B=sub_B, C=C)
    expect_that((sub_A[,] %*% sub_B[,]), is_equivalent_to(C[,]))
    
    # when C is a sub.big.matrix
    A <- create_big_matrix(xdim=20, ydim=10)
    B <- create_big_matrix(xdim=10, ydim=20)
    C <- big.matrix(nrow(A), ncol(B)+10, type="double")
    sub_C <- sub.big.matrix(C, firstCol=5, lastCol=ncol(B)+4)
    dgemm(A=A, B=B, C=sub_C)
    expect_that((A[,] %*% B[,]), is_equivalent_to(sub_C[,]))
})

