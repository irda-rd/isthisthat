#' @title Identify valid dates
#' @description Identify if elements of a character vector represent valid dates in the designated format.
#' @param x \code{character} vector.
#' @param format \code{character} representing a date (see \code{as.Date}). The only implemented format is \code{"\%Y-\%m-\%d"}.
#' @return Return a \code{logical} vector identifying, for each element, if the string correspond to a valid date in the given format.
#' @export
#' @examples
#' x <- c("2019-06-20", "2019-06-32", "43636", NA, "2019/06/20")
#' isStringADate(x)
#'
isStringADate <- function(x, format = "%Y-%m-%d"){
  #Definition of the yyyy-mm-dd pattern for grepl
  year <- "\\d{4}"
  month <- "\\d{2}"
  day <- "\\d{2}"
  ymdPattern <- paste0("^", year, ".*", "\\-", ".*", month, ".*", "\\-", ".*", day, "$")

  #Identify elements that respect this pattern
  logicalYMD <- grepl(ymdPattern, x)

  #Also check that the pattern correspond to a valid date
  logicalDate <- logicalYMD  & !is.na(as.Date.character(as.character(x), format = format))
  return(logicalDate)
}


