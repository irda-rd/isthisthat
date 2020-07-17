#' @title Approximative equality
#' @description Check approximative equality for the \code{numeric} class, including \code{NA} presence.
#' @param x, object 1 to compare (vector, \code{matrix}, \code{data.frame})
#' @param y, object 2 to compare (vector, \code{matrix}, \code{data.frame})
#' @param tol tolerance used when considering numeric values equal.
#' @details The length or dimension of \code{x} and \code{y} must be the same. The function allows to determine if the difference between two numeric is smaller than \code{tol}. It also allows to compare a \code{numeric} with \code{character} representing a number. The latter, if corresponding to a number, are converted to numeric then rounded at 15 significative numbers (the recommanded limit for doubles in the IEEE-754 standard). Comparison is made as \code{character} to avoid coercion of the initial character vector. Scientific notation is also handled.
#' The approach allows to avoid truncating problems associated with \code{as.character} for number with several decimals, precision problems with \code{numeric} as well as notation problems. The function refer to \code{isEqual} for any other case.
#' @return If \code{x} and \code{y} are vectors, a \code{logical} vector of the same length is returned; if \code{x} and \code{y} are of class \code{matrix} or \code{data.frame}, a \code{logical matrix} of the same dimension is returned.
#' @import stringr
#' @export
#' @examples
#' #Comparison between isEqual and isAlmostEqual
#' ##For two numeric vectors 
#' ##(4th case is not equal, but is almost equal given the tol)
#' x <- c(NA, 1, 1.1, 1.000000001, 123456789.123456789, 1000000)
#' y <- c(NA, 1, 1.1, 1.000000002, 123456789.123456789, 1000000)
#' isEqual(x, y)
#' isAlmostEqual(x, y)
#'
#' #For a numeric and a character vector 
#' ##(5th case have more than 15 significative numbers, 6th case is a change in notation)
#' xChar <- c(NA, "1", "1.1", "1.000000001", "123456789.123456789", "1000000")
#' isEqual(x, xChar)
#' isAlmostEqual(x, xChar)
#'
#' #With two data frames
#' df1 <- data.frame(x1 = x, x2 = x, x3 = letters[1:6], stringsAsFactors = FALSE)
#' df2 <- data.frame(x1 = y, x2 = xChar, x3 = letters[1:6], stringsAsFactors = FALSE)
#' isEqual(df1, df2)
#' isAlmostEqual(df1, df2)
#'
#' #With two matrices
#' M1 <- matrix(x,3,2)
#' M2 <- matrix(xChar,3,2)
#' isEqual(M1, M2)
#' isAlmostEqual(M1, M2)
#'
#' #With scientific notation
#' isEqual("1.234E-1",0.1234)
#' isAlmostEqual("1.234E-1",0.1234)
#'
setGeneric("isAlmostEqual", function(x, y, ...){
standardGeneric("isAlmostEqual")
})
setMethod("isAlmostEqual", signature(x = "data.frame", y = "data.frame"), function(x, y, tol= .Machine$double.eps^0.5){
  #Class indentification
  classex <- sapply(x, class)
  classey <- sapply(y, class)
  logicalColNum <- classex == "numeric" | classey == "numeric"

  #Check equality (by column if numeric, global otherwise)
  logicalEqual <- matrix(logical(nrow(x) * ncol(x)), ncol = ncol(x), nrow = nrow(x))
  ##Numeric case (dispatch on itself for ignature numeric)
  iNum <- which(logicalColNum)
  for (i in iNum){
    logicalEqual[, i] <- isAlmostEqual(x[, i], y[, i], tol)
  }

  ##Non numerical case (call isEqual to fasten execution)
  iNonNum <- as.integer(which(!logicalColNum))
  logicalEqual[, iNonNum] <- isEqual(x[, iNonNum, drop = FALSE], y[, iNonNum, drop = FALSE])
  return(logicalEqual)
})

setMethod("isAlmostEqual", signature(x = "matrix", y = "matrix"), function(x, y, tol= .Machine$double.eps^0.5){
  #Save dimension and convert into vectors (avoid the call to a data.frame to fasten execution)
  dim <- dim(x)
  x <- as.vector(x)
  y <- as.vector(y)

  #Check equality as a vector, then convert as a logical matrix
  logicalEqualVector <- isAlmostEqual(x, y, tol)
  logicalEqual <- matrix(logicalEqualVector, nrow = dim[1], ncol = dim[2])
  return(logicalEqual)
})

setMethod("isAlmostEqual", signature(x = "numeric", y = "numeric"), function(x, y, tol=.Machine$double.eps^0.5){
  #Two numeric (NA and tolerance)
  logicalSameNA <- is.na(x) == is.na(y)
  logicalBothNA <- is.na(x) & logicalSameNA

  logicalTolerance <- abs(x - y) < tol
  logicalSameNNA <- !is.na(logicalTolerance) & (logicalTolerance)

  logicalEqual <- (logicalBothNA | logicalSameNNA)
  return(logicalEqual)
})

setMethod("isAlmostEqual", signature(x = "character", y ="numeric"), function(x, y, tol = .Machine$double.eps^0.5){
  #Dispatch on an equivalent signature
  logicalEqual <- isAlmostEqual(y, x, tol)
  return(logicalEqual)
})

setMethod("isAlmostEqual", signature(x = "numeric", y = "character"), function(x, y, tol = .Machine$double.eps^0.5){
  #Rounding and character formating (scientific representation)
  ##Numeric part
  logicalNNA <- !(is.na(x))
  xChar <- rep(NA, length(x))
  x[logicalNNA] <- signif(x[logicalNNA], digits = 15) #15-17 significant digits for double in the IEEE-754 standard
  xChar[logicalNNA] <- formatC(x[logicalNNA], format = "e", digits = 15-1)
  xChar <- str_trim(xChar)

  ##Character part
  logicalDecimal <- isStringANumber(y)
  y[logicalDecimal] <- formatC(as.numeric(y[logicalDecimal]), format = "e", digits = 15-1)
  y <- str_trim(y)

  #Check equality (as character)
  logicalEqual <- isEqual(xChar, y)
  return(logicalEqual)
})

setMethod("isAlmostEqual", signature(x = "ANY", y = "ANY"), function(x, y, tol = .Machine$double.eps^0.5){
  logicalEqual <- isEqual(x, y)
  return(logicalEqual)
})


