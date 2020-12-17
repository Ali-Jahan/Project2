library(randomForest)
library(dplyr)
library(class)
library(palmerpenguins)
# set random seed for reproducibility
set.seed(1234)
# omit NAs
penguins <- na.omit(penguins)


test_that("Testing error in case of non-numeric input", {
  expect_error(my_rf_cv("a string"))
})

test_that("my_rf_cv is returning the proper output", {
  expect_is(my_rf_cv(5), 'numeric')
})
