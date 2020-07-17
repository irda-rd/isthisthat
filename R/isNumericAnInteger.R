#' @title Identify if numbers are integers
#' @description Identify if elements of a \code{numeric} vector are integers.
#' @param x vector of class \code{numeric}
#' @param tol tolerance used when considering numeric values equal.
#' @details The function also return \code{FALSE} for \code{NA}. Code in part taken from the example of the function \code{integer}.
#' @return Return a \code{logical} vector indicating for each element if it correspond to an integer.
#' @export
#' @examples
#' x <- c(1, 1.1, 0.1, 1.0, 1.00000001, NA)
#' isNumericAnInteger(x)
#'
isNumericAnInteger <- function(x, tol = .Machine$double.eps^0.5) {
  logicalInteger <- !is.na(x) & abs(x - round(x, 0)) < tol
  return(logicalInteger)
}

