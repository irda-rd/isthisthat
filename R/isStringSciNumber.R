#' @title Identify if a string is a scientific number
#' @description Check if elements of a \code{character} vector are numbers expressed in scientific notation.
#' @param x \code{character} vector.
#' @param decimalSeparator \code{character} used as a decimal separator ("," or "."); \code{decimalSeparator = "."} by default.
#' @return Return a \code{logical} vector indicating, for each element of \code{x}, if the string correspond to a number in scientific notation.
#' @details The function can identify positive or negative numbers, integer or decimals, followed by \code{E} (or \code{e}), another symbol (\code{+}, \code{-} or nothing) and digits representing the exponent.
#' @export
#' @examples
#' x <- c("1234", "-1","+1", "12.34","12,34", "123A","-1.1","E+1")
#' y <- c("1E+1","-1e-1", "+1.1e-1","1.1E10","1,1e+10")
#' isStringSciNumber(x)
#' isStringSciNumber(y)
#' isStringSciNumber(y, decimalSeparator = ",")
#'
isStringSciNumber <- function(x, decimalSeparator = "."){
  sign <- "([+]|[-])?"
  digits <- "(\\d)+"
  exponent <- paste0("([Ee]",sign,"(\\d)+){1}")
  separator <- paste0("(\\",decimalSeparator,"{1})")

  significandInt <- digits
  significandDec <- paste0(digits, separator, digits)

  exprInt <- paste0("^",sign, significandInt, exponent,"$")
  exprDec <- paste0("^",sign, significandDec, exponent,"$")

  logicalSci <- grepl(exprInt, x) | grepl (exprDec, x)
  return(logicalSci)
}

