#' @title Verify equality
#' @description Verify equality, including \code{NA} presence.
#' @param x, object 1 to compare (vector, \code{matrix}, \code{data.frame})
#' @param y, object 2 to compare (vector, \code{matrix}, \code{data.frame})
#' @return If \code{x} and \code{y} are vectors, a \code{logical} vector of the same length is returned; if \code{x} and \code{y} are of class \code{matrix} or \code{data.frame}, a \code{logical} matrix of the same dimension is returned.
#' @details The length or dimension of \code{x} and \code{y} must be the same. The function rely on the \code{==} operator; if \code{x} or \code{y} are \code{numeric} with several significative numbers, one should use the function \code{isAlmostEqual} instead.
#' @export
#' @examples
#' x <- c(1:3,NA)
#' y <- c("1","C",NA, NA)
#' isEqual(x, y)
isEqual <- function(x, y){
  logicalSameNA <- is.na(x) == is.na(y)
  logicalBothNA <- is.na(x) & logicalSameNA
  logicalSameNNA <- !is.na(x == y) & (x == y)
  logicalEqual <- (logicalBothNA | logicalSameNNA)
  return(logicalEqual)
}

