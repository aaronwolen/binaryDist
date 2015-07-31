.onUnload <- function(libpath) {
  library.dynam.unload("binaryDist", libpath)
}