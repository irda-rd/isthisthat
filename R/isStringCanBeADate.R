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
#----------------------------------------------------------
# #' @title Conversion of decimal separator
# #' @description Convert decimal separator for string that represent a number.
# #' @param x vector of class \code{character}
# #' @param from \code{character}, initial separator. By default \code{from = ","}.
# #' @param to \code{character}, final separator. By default \code{to = "."}.
# #' @return Return \code{x} with decimal separator converted.
# #' @details No change is brought if \code{x} is not of class \code{character}.
# #' @examples
# #' x <- c("12", "1.2", "1,2", "1,A")
# #' convDecimalSeparator (x)
# #' convDecimalSeparator (x, from = ".", to = ",")
# #'
convDecimalSeparator <- function(x, from = ",", to = "."){
  if(class(x) == "character"){
    iFromNumber <- isStringANumber(x, from)
    x[iFromNumber] <- gsub(pattern = paste0("\\", from), replacement = paste0("\\", to), x[iFromNumber])
  }
  return(x)
}
