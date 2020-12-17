# Function: my_t.test, implements a t-test
# Input:    x     : Numeric vector
#           alternative : the mode for t test (two sided, lesser, greater)
#           mu: Numeric of length 1
# Output:   returns a List with following components:
#           test stat, degree of freedom, alternative, p value

#' T hypothesis test.
#'
#' This function implements one sided and two sided T hypothesis tests.
#'
#' @param k Numeric input indicating number of folds in cross validation.
#' @keywords prediction
#'
#' @return Numeric with cross validation error of performing random forest,
#'   using \code{k} folds cross validation.
#'
#' @examples
#' library(randomForest)
#' library(dplyr)
#' library(class)
#' library(palmerpenguins)
#' # set random seed for reproducibility
#' set.seed(1234)
#' # omit NAs
#' penguins <- na.omit(penguins)
#' # use function
#' my_rf_cv(5)
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
