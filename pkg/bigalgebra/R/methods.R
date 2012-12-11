setMethod("%*%",signature(x="big.matrix", y="big.matrix"),
  function(x,y) dgemm(A=x, B=y),
  valueClass="big.matrix"
)

setMethod("%*%",signature(x="matrix", y="big.matrix"),
  function(x,y)
  {
    if(dim(x)[2] != dim(y)[1]) stop("non-conformant matrices")
    R = options("bigalgebra.mixed_airthmetic_returns_R_matrix")[[1]]
    if(!is.null(R) && R) return(dgemm(A=x, B=y, C=0))
    dgemm(A=x, B=y)
  },
  valueClass="matrix"
)

setMethod("%*%",signature(x="big.matrix", y="matrix"),
  function(x,y) 
  {
    if(dim(x)[2] != dim(y)[1]) stop("non-conformant matrices")
    R = options("bigalgebra.mixed_airthmetic_returns_R_matrix")[[1]]
    if(!is.null(R) && R) return(dgemm(A=x, B=y, C=0))
    dgemm(A=x, B=y)
  },
  valueClass="matrix"
)
