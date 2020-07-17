#' @title Identify if a string is a number
#' @description Check if elements of a \code{character} vector are numbers, in their scientific notation or not.
#' @param x \code{character} vector.
#' @param decimalSeparator \code{character} used as a decimal separator ("," or "."); \code{decimalSeparator = "."} by default.
#' @return Return a \code{logical} vector indicating, for each element of \code{x}, if the string correspond to a number.
#' @export
#' @examples
#' x <- c("1234", "12.34","1.1E+10", "12,34", "1,1E+10", "123A")
#' isStringANumber(x,".")
#' isStringANumber(x,",")
isStringANumber <- function(x, decimalSeparator =".") {
  logicalDecimal <- isStringADecimal(x, decimalSeparator = decimalSeparator)
  logicalInteger <- isStringAnInteger(x)
  logicalSciNumber <- isStringSciNumber(x, decimalSeparator = decimalSeparator)
  logicalNumber <- logicalDecimal | logicalInteger | logicalSciNumber
  return(logicalNumber)
}
