# Creates input for different tests
create_big_matrix <- function(xdim=20, ydim=10) {
    as.big.matrix(matrix(rnorm(xdim*ydim), xdim, ydim), type="double")
}
