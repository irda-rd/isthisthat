#' @title Identify if a string can be converted into a date
#' @description Check if a vector, \code{matrix} or \code{data.frame} of class \code{numeric} or \code{character} can be converted into a \code{Date}.
#' @param x vecteur, \code{matrix} ou \code{data.frame} de \code{character} ou \code{numeric}.
#' @return Return a \code{logical} vector indicating, for each element of \code{x}, if the string can be converted into a \code{Date}.
#' @details String representing valid dates, according to the specified format, numbers and \code{NA} (but not "NA") are considered to be convertible into a \code{Date}.
#' @export
#' @examples
#' #Examples of valid cases
#' x <- c("2019-06-20", 43636, "43636.1", "43636,1", NA)
#' isStringCanBeADate(x)
#'
#' #Examples of non-valid cases
#' y<- c("AA", "2012/05/02", "06-05-2012", "2019-06-32", "NA")
#' isStringCanBeADate(y)
#'
isStringCanBeADate <- function(x, origin = as.Date("1899-12-30"), format=("%Y-%m-%d"), warning = FALSE){
  #Categorisation of entries
  ##Identification of numbers (integers or decimals)
  x <- convDecimalSeparator(x)
  logicalNumber <- isStringANumber(x, ".")

  ##Identification of dates in the specified format
  logicalDate <- isStringADate(x, format = format)

  ##Cases already NA
  logicalNA <- is.na(x)

  ##Identification of other cases and warning
  logicalCanBeADate <- (logicalNumber | logicalDate | logicalNA)
  if(warning){
    if(length(which(!logicalCanBeADate)) > 0){
      warning("Elements does not correspond to the specified date format, coercion as NA")
    }
  }
  return(logicalCanBeADate)
}


