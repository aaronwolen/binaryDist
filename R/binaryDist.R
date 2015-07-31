#' Calculate binary distance on a matrix
#' 
#' @param x a numeric matrix
#' 
#' @export
binaryDist <- function(x) {
  if (class(x) != "PackedMatrix") {
    x <- as.PackedMatrix(x)
  }
  dst <- bDist(x)
  attr(dst, "Size") <- ncol(x)
  attr(dst, "Diag") <- attr(dst, "Upper") <- FALSE
  attr(dst, "method") <- "binary"
  attr(dst, "call") <- match.call()
  class(dst) <- "dist"
  dst
}
