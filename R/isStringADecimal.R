#' @title Identify if a string is a decimal number
#' @description Check if elements of a \code{character} vector are decimal numbers (i.e. separated by a decimal symbol).
#' @param x \code{character} vector.
#' @param decimalSeparator \code{character} used as a decimal separator ("," or "."); \code{decimalSeparator = "."} by default.
#' @return Return a \code{logical} vector indicating, for each element of \code{x}, if the string correspond to a decimal number.
#' @details The function does not identify number in their scientific notation. Use the function \code{isStringSciNumber} for this purpose.
#' @export
#' @examples
#' x <- c("12.34", "12.00", "-1.1", "12,34", "-1.1E-2", "1234", "-1", "1E-2", "123A", "12.00.00", ".1")
#' isStringADecimal(x,".")
#' isStringADecimal(x,",")
isStringADecimal <- function(x, decimalSeparator = "."){
  sign <- "([+]|[-])?"
  digits <- "(\\d)+"
  separator <- paste0("(\\",decimalSeparator,"{1})")
  expr <- paste0("^",sign, digits, separator, digits,"$")
  logicalDecimal <- grepl(expr, x)
  return(logicalDecimal)
}

