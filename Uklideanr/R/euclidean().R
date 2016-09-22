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
  if (length(a) > 1 || length(b) > 1) {
    stop("a and b can only be vectors with the length 1.")
  }
  
  if (!is.numeric(a) || !is.numeric(b)) {
    stop("a and b have to be numerical.")
  }
  
  if (a == 0 && b == 0) {
    stop("GCD for both zeros not defined.")
  }
  else {
    while (b != 0) {
    t <- b
    b <- a %%b
    a <- t
  }
  return (a)
  }
}