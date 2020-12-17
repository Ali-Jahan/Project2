x <- rnorm(100, 0, 1)
y <- 12 * x + 5
# formula
formula <- y ~ x
data <- data.frame("x" = x, "y" = y)

test_that("Error if incorrect input data type", {
  expect_error(my_lm("a string", formula))
})

test_that("Error if incorrect input formula type", {
  expect_error(my_lm(data, "a string"))
})

test_that("Correct Output Format", {
  expect_is(my_lm(data, formula), "list")
})

