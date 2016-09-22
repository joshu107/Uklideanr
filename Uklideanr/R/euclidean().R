#' Uses the Euclidean Algorithm to find the greatest common divisor of two integers.
#' @param a An integer.
#' @param b An integer.
#' @return Returns greatest common divisor of \code{a} and \code{b}.
#' @examples
#' euclidean(4, 64)
#' euclidean(17, 3)
#' euclidean(16, 16)
#' @references https://en.wikipedia.org/wiki/Euclidean_algorithm
euclidean <- function (a, b) {
  if (!is.integer(a) | (!is.integer(b))) {
    stop("Both input variables must be integers!")
  }
  while (b != 0) {
    t <- b
    b <- a %%b
    a <- t
  }
  return (a)
}