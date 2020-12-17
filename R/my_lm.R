#' Linear Model Function
#'
#' This function fits a linear model on a given data.
#'
#' @param data Numeric input of the data set.
#' @param formula Formula input indicating the model.
#' @keywords inference
#'
#' @return List including \code{model}, \code{response}, \code{coefficients},
#'   \code{df} for degree of freedom, and \code{std_error} of the fit linear model.
#'
#' @examples
#' set.seed(1234)
#' # some test data
#' x <- rnorm(100, 0, 1)
#' y <- 12 * x + 5
#' # formula
#' formula <- y ~ x
#' data <- data.frame("x" = x, "y" = y)
#' # call function
#' my_linear_model <- my_lm(data, formula)
#'
#' @export
my_lm <- function(data, formula) {
  # model frame
  model_frame <- model.frame(formula, data)
  # model matrix of X
  X <- model.matrix(formula, data)
  # response Y
  Y <- model.response(model_frame)
  # solve for coefficients
  # beta vector
  B <- solve(t(X) %*% X) %*% t(X) %*% Y
  # degree of freedom
  # uses terms's attribute to get number of terms in formula
  # then adds one for intercept
  degree_freedom <- length(attr(terms(formula), 'term.labels')) + 1
  # sigma^2
  sigma_2 <- sum((Y - (X %*% B)) / degree_freedom)
  # standard error
  std_error <- diag(length(B))
  std_error <- std_error * sigma_2
  # output list
  output_list <- list("model" = X,
                      "response" = Y,
                      "coefficients" = t(B),
                      "df" = degree_freedom,
                      "std_error" = std_error)
  return(output_list)
}


