# This function is used to match up a vector of column names to the
# entire set of column names, providing the proper column indices.
# The name choice was based on the phrase "multiple map" though
# perhaps we should have made a different choice. - JE 1/16/08

# Sorry, the name choice wasn't great.

mmap = function(x, y) {
  if (is.null(x)) return(NULL)
  ans <- match(x, y)
  if (any(is.na(ans))) stop("Couldn't find a match to one of the arguments.")
  return(ans)
}

