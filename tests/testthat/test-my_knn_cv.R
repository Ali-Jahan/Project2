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

test_that("function's CV Error works as expected", {
  expect_true(abs(output$cv_err - 0.1650384) < tol)
})

test_that("function's  output is in correct format", {
  expect_is(output, "list")
})
