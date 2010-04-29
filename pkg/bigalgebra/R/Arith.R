
setMethod("+", signature(e1="big.matrix", e2="big.matrix"),
  function(e1, e2) {
    return(daxpy(NULL, 1, e1, 1, e2, 1))
  })
