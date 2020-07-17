#' @title Identify if a string is an integer
#' @description Check if elements of a \code{character} vector are integers
#' @param x \code{character} vector
#' @return Return a \code{logical} vector indicating, for each element of \code{x}, if the string correspond to an integer.
#' @details Use the function \code{isStringSciNumber} to identify numbers in their scientific notation.
#' @export
#' @examples
#' x <- c("1234", "-1","+1", "12.34","12,34", "123A","-1.1","1E+1")
#' isStringAnInteger(x)
isStringAnInteger <- function(x) {
  sign <- "([+]|[-])?"
  digits <- "(\\d)+"
  expr <- paste0("^",sign,digits,"$")
  logicalInteger <- grepl(expr, x)
  return(logicalInteger)
}


