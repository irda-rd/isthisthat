#' @description The package check the representation and equality of strings and numbers. All functions determine if an object \emph{this} is of the form or equal to \emph{that} and return a \code{logical} object of the same dimension.
#' \cr\cr
#' More precisely, a group of functions (\code{\link{isStringAnInteger}}, \code{\link{isStringADecimal}}, \code{\link{isStringASciNumber}}, \code{\link{isStringANumber}}) verify if an object of class \code{character} correspond to certain types of number, while \code{\link{isStringADate}} and \code{\link{isStringCanBeADate}} concern the relation between \code{character} and \code{Date}.
#' \cr\cr
#' Similarly, the functions \code{\link{isNumericADecimal}} and \code{\link{isNumericAnInteger}} allow verifications to be made on objects of class \code{numeric}.
#' \cr\cr
#' Finally, the functions \code{\link{isEqual}} and \code{\link{isAlmostEqual}} allow to compare objects with \code{NA} and according to some tolerance.
#' \cr\cr
#' The name is a reference to \emph{This Is That}, the news satire program broadcasted on CBC Radio.
"_PACKAGE"
