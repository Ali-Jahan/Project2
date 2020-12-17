#' T hypothesis test.
#'
#' This function implements one sided and two sided T hypothesis tests.
#'
#' @param x Numeric input of the data set.
#' @param alternative String input indicating whether the hypothesis is "less", "greater", or "two-sided".
#' @param mu Numeric indicating the mean (mu) that is being tested against.
#' @keywords inference
#'
#' @return List including \code{test_stat}, \code{df} for degree of freedom, \code{alternative},
#'   and \code{p_val} of T hypothesis test.
#'
#' @examples
#' set.seed(1234)
#' # some test data
#' test_data <- rnorm(100, 0, 1)
#' # one sided less
#' my_t.test(test_data, alternative = "less", mu = 0)
#' # one sided greater
#' my_t.test(test_data, alternative = "greater", mu = 0)
#' # two sided
#' my_t.test(test_data, alternative = "two.sided", mu = 0)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # input checking for x to be a numeric vector
  if(!is.numeric(x)) {
    stop('Input data must be a numeric vector.')
  }
  # input check for mu - must be numeric of length 1
  if(!is.numeric(mu) || length(mu) > 1) {
    stop('the field \"mu\" must be numeric of length 1.')
  }

  # degree of freedom
  degree_freedom <- length(x) - 1
  # size of data set
  n <- length(x)
  # standard error
  std_err <- sd(x) / sqrt(n)
  # mean of input vector
  mean_x <- mean(x)
  # test stat
  test_stat <- (mean_x - mu) / std_err
  # temp p value
  p_val <- 0
  # checking the "alternative" field
  if(alternative == "two.sided") {        # two sided t test
    p_val <- 2 * pt(abs(test_stat), degree_freedom, lower.tail = FALSE)
  } else if(alternative == "less") {      # less t test
    p_val <- pt(test_stat, degree_freedom, lower.tail = TRUE)
  } else if(alternative == "greater") {   # greater t test
    p_val <- pt(test_stat, degree_freedom, lower.tail = FALSE)
  } else {
    # wrong input ---> terminate and give error message
    stop('field \"alternative\" must be two.sided, less, or greater')
  }
  # creating the output list
  output_list <- list("test_stat" = test_stat,
                      "df" = degree_freedom,
                      "alternative" = alternative,
                      "p_val" = p_val)
  return(output_list)
}
