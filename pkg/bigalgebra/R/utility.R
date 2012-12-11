
is_transposed = function( tcode )
{
  if ( sum(tcode == c('n', 'N')) > 0 )
    return(FALSE)
  if ( sum(tcode == c('T', 't', 'C', 'c')) > 0 )
    return(TRUE)
  stop("Invalid transpose code given") 
}

check_matrix = function(A, classes=c('big.matrix', 'matrix'), 
  types='double')
{
  if (!any( class(A) == classes))
  {
    stop("A is not the correct class type")
  }
  if (!any(typeof(A) == types))
  {
    stop("The matrix type is not correct")
  }
  return( ifelse( class(A) == 'big.matrix', TRUE, FALSE ) )
}

# Create a big.matrix of the specified dimensions using the
# options(bigalgebra.temp_pattern) and options(bigalgebra.tempdir) naming
# convention. Such big.matrices are often used to store computed output.
anon_matrix = function(m, n, type)
{
  if(missing(type)) type = options("bigmemory.default.type")
  f = basename(tempfile(pattern=options("bigalgebra.temp_pattern")[[1]]))
  p = options("bigalgebra.tempdir")[[1]]()
  d = sprintf("%s.desc",f)
  filebacked.big.matrix(m, n, type, backingfile=f, backingpath=p,
                          descriptorfile=d)
}
