## usethis namespace: start
#' @useDynLib SIT, .registration = TRUE
## usethis namespace: end
NULL

## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL

.onUnload <- function (libpath) {
  library.dynam.unload("SIT", libpath)
}
