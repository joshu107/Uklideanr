.libPaths("Z:/Documents/R")
euclidean <- function (a, b) {
  while (b != 0) {
    t <- b
    b <- a %%b
    a <- t
  }
  return (a)
}