#' Generate a random sparse matrix for testing purposes
#' 
#' @param nObs
#' @param nVariables
#' @param maxBits
#' @param packed 
#' 
#' @export
makeRandomData <- function(nObs, nVariables, maxBits, packed = FALSE) {
  x <- replicate(nObs, {
    y <- integer(nVariables)
    y[sample(nVariables, sample(maxBits, 1))] <- 1L
    if (packed) {
      packRow(y, 64L)
    } else {
      y
    }
  })
  if (packed) {
    class(x) <- "PackedMatrix"
    x
  } else {
    t(x)
  }
}