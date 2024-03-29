% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/isAlmostEqual.R
\name{isAlmostEqual}
\alias{isAlmostEqual}
\title{Approximative equality}
\usage{
isAlmostEqual(x, y, ...)
}
\arguments{
\item{x}{object 1 to compare (vector, \code{matrix}, \code{data.frame}).}

\item{y}{object 2 to compare (vector, \code{matrix}, \code{data.frame}).}

\item{tol}{tolerance used when considering numeric values equal.}
}
\value{
If \code{x} and \code{y} are vectors, a \code{logical} vector of the same length is returned; if \code{x} and \code{y} are of class \code{matrix} or \code{data.frame}, a \code{logical matrix} of the same dimension is returned.
}
\description{
Check approximative equality for the \code{numeric} class, including \code{NA} presence.
}
\details{
The length or dimension of \code{x} and \code{y} must be the same. The function allows to determine if the difference between two numeric is smaller than \code{tol}. It also allows to compare a \code{numeric} with \code{character} representing a number. The latter, if corresponding to a number, are converted to numeric then rounded at 15 significative numbers (the recommanded limit for doubles in the IEEE-754 standard). Comparison is made as \code{character} to avoid coercion of the initial character vector. Scientific notation is also handled.
The approach allows to avoid truncating problems associated with \code{as.character} for number with several decimals, precision problems with \code{numeric} as well as notation problems. The function refer to \code{isEqual} for any other case.
}
\examples{
#Comparison between isEqual and isAlmostEqual
##For two numeric vectors 
##(4th case is not equal, but is almost equal given the tol)
x <- c(NA, 1, 1.1, 1.000000001, 123456789.123456789, 1000000)
y <- c(NA, 1, 1.1, 1.000000002, 123456789.123456789, 1000000)
isEqual(x, y)
isAlmostEqual(x, y)

#For a numeric and a character vector 
##(5th case have more than 15 significative numbers, 6th case is a change in notation)
xChar <- c(NA, "1", "1.1", "1.000000001", "123456789.123456789", "1000000")
isEqual(x, xChar)
isAlmostEqual(x, xChar)

#With two data frames
df1 <- data.frame(x1 = x, x2 = x, x3 = letters[1:6], stringsAsFactors = FALSE)
df2 <- data.frame(x1 = y, x2 = xChar, x3 = letters[1:6], stringsAsFactors = FALSE)
isEqual(df1, df2)
isAlmostEqual(df1, df2)

#With two matrices
M1 <- matrix(x,3,2)
M2 <- matrix(xChar,3,2)
isEqual(M1, M2)
isAlmostEqual(M1, M2)

#With scientific notation
isEqual("1.234E-1",0.1234)
isAlmostEqual("1.234E-1",0.1234)

}
