#' Calculate binary distance on a matrix
#' 
#' @param x a numeric matrix
#' 
#' @export
binaryDist <- function(x) {
  labels <- rownames(x)
  if (class(x) != "PackedMatrix") x <- as.PackedMatrix(x)
  
  dst <- bDist(x)
  attr(dst, "Size") <- ncol(x)
  if (!is.null(labels)) attr(dst, "Labels") <- labels
  attr(dst, "Diag") <- attr(dst, "Upper") <- FALSE
  attr(dst, "method") <- "binary"
  attr(dst, "call") <- match.call()
  class(dst) <- "dist"
  dst
}
