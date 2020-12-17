set.seed(1234)
# some test data
test_data <- rnorm(100, 0, 1)
result <- my_t.test(test_data, "less", 0)

test_that("Test correct output format", {
  expect_is(result, "list")
})

test_that("Test error if input data is not numeric", {
  expect_error(my_t.test("a string", "less", 0))
})

test_that("Test error if alternative is not string", {
  expect_error(my_t.test(test_data, c(2,1,9), 0))
})

test_that("Test error if mu is not numeric", {
  expect_error(my_t.test(test_data, "less", "a string"))
})

test_that("Test DF value", {
  expect_equal(result$df, 99)
})
