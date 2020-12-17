#' Random Forest Cross-Validation
#'
#' This function implements the Random Forest function for penguins data set, with cross validation.
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
my_rf_cv <- function(k) {
  # constructing the input data frame using only the
  # columns of interest
  train <- data.frame(body_mass_g = penguins$body_mass_g,
                      bill_length_mm = penguins$bill_length_mm,
                      bill_depth_mm = penguins$bill_depth_mm,
                      flipper_length_mm = penguins$flipper_length_mm)
  # number of training
  n <- nrow(train)
  # random assignment of folds
  fold <- sample(rep(1:k, length = n))
  # add split indexes to the train data and class
  data <- train
  data$split <- fold
  # vector for cross validation errors
  errors <- numeric(length = k)
  for (i in 1:k) {
    # split train and test data
    data_train <- data %>% filter(split != i)
    data_test <- data %>% filter(split == i)
    # perform random forest
    model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm,
                          data = data_train, ntree = 100)
    # get predictions based on model
    predictions <- predict(model, data_test[, 2:4])
    # compute MSE error
    mse_err <- mean((data_test$body_mass_g - predictions) ^ 2)
    # insert into the vector of mse errors
    errors[i] <- mse_err
  }
  return(mean(errors))
}
