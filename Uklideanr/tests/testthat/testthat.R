library(Uklideanr)
context("Input parameters of length one.")

num_vec <- c(1, 2, 3)
char_vec <- c("a", "b", "c")
bool_vec <- c(TRUE, FALSE, TRUE)

test_that("Vector is not of length one.", {
  expect_error(euclidean(num_vec, num_vec), "a and b can only be vectors with the length 1.")
  expect_error(euclidean(num_vec, 5), "a and b can only be vectors with the length 1.")
  expect_error(euclidean(5, num_vec), "a and b can only be vectors with the length 1.")
  expect_error(euclidean(num_vec, char_vec), "a and b can only be vectors with the length 1.")
  expect_error(euclidean(num_vec, bool_vec), "a and b can only be vectors with the length 1.")
})


context("Numerical input parameter")

test_that("Input is not numerical.", {
  expect_error(euclidean("a", "b"), "a and b have to be numerical.")
  expect_error(euclidean(5, "b"), "a and b have to be numerical.")
  expect_error(euclidean("a", 5), "a and b have to be numerical.")
  expect_error(euclidean(TRUE, FALSE), "a and b have to be numerical.")
  expect_error(euclidean(TRUE, 5), "a and b have to be numerical.")
  expect_error(euclidean(5, FALSE), "a and b have to be numerical.")
})

context("a or b is zero")

test_that("a, b = 0", {
  expect_error(euclidean(0,  0), "GCD for both zeros not defined.")
})

test_that("a or b = 0", {
  expect_that(euclidean(10, 0), equals(10))
  expect_that(euclidean(0, 10), equals(10))  
})

context("Check results")

test_that("Check results don't match task.", {
  expect_equal(euclidean(123612, 13892347912), 4)
  expect_equal(euclidean(100, 1000), 100)
})