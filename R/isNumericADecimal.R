#' @title Identify if numbers are decimal numbers
#' @description Identify if elements of a \code{numeric} vector are decimal numbers.
#' @param x vector of class \code{numeric}
#' @param tol tolerance used when considering numeric values equal.
#' @details The function also return \code{FALSE} for \code{NA}. The code is in part taken from the example of the function \code{integer}.
#' @return Return a \code{logical} vector indicating for each element if it correspond to a decimal number.
#' @export
#' @examples
#' x <- c(1, 1.1, 0.1, 1.0, 1.00000001, NA)
#' isNumericADecimal(x)
#'
isNumericADecimal <- function(x, tol = .Machine$double.eps^0.5) {
  logicalDecimal <- !is.na(x) & abs(x - round(x, 0)) > tol
  return(logicalDecimal)
}
