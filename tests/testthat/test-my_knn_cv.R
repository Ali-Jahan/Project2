library(dplyr)
library(class)
library(palmerpenguins)
# set random seed for reproducibility
set.seed(1234)
# omit NAs
penguins <- na.omit(penguins)
# constructing the input data frame using only the
# columns of interest
train_ <- data.frame("bill_length_mm" = penguins$bill_length_mm,
                     "bill_depth_mm" = penguins$bill_depth_mm,
                     "flipper_length_mm" = penguins$flipper_length_mm,
                     "body_mass_g" = penguins$body_mass_g)
# constructing the true value of class data
cl_ <- data.frame("species" = penguins$species)
output <- my_knn_cv(train_, cl_, 1, 5)
class_ <- output$class
misclass_ <- sum(output$class != cl_[, 1]) / nrow(cl_)
tol <- 1e-5

test_that("my_knn_cv's CV Error works as expected", {
  expect_true(abs(output$cv_err - 0.1650384) < tol)
})

test_that("my_knn_cv's  output is in correct format", {
  expect_is(output, "list")
})

test_that("testing error in case of wrong input data set", {
  expect_error(my_knn_cv("a string", cl_, 1, 5))
})

test_that("testing error in case of wrong input true values", {
  expect_error(my_knn_cv(train_, "a string", 1, 5))
})

test_that("testing error in case of wrong input for k_nn", {
  expect_error(my_knn_cv(train_, cl_, "a string", 5))
})

test_that("testing error in case of wrong input for k_cv", {
  expect_error(my_knn_cv(train_, cl_, 1, "a string"))
})
