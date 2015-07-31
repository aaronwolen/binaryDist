#' binaryDist: Efficient binary/Jaccard distance
#' 
#' binaryDist provides performs fast binary/Jaccard distance calculations on a
#' matrix. It uses raw vectors with the binary data efficiently packed 8 bits to
#' a byte and makes use of 64 bit operators.
#' 
#' @docType package
#' @name binaryDist
#' @useDynLib binaryDist
#' @importFrom Rcpp sourceCpp
NULL

