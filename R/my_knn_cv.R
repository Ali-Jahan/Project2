#' k-Nearest Neighbors Cross-Validation
#'
#' This function implements the k nearest neighbors with cross validation
#'
#' @param train Data frame of training set.
#' @param cl Data frame of the true class values of training data.
#' @param k_nn Numeric input indicating number of neighbors
#' @param k_cv Numeric input indicating the number of folds
#' @keywords knn
#'
#' @return List object with \code{class} as a vector of predicted class
#'   for all observations, and \code{cv_err} as a numeric with the
#'   cross-validation misclassification error
#'
#' @examples
#' my_knn_cv(train_df, class_df, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # number of training
  n <- nrow(train)
  # random assignment of folds
  fold <- sample(rep(1:k_cv, length = n))
  # add split indexes to the train data and class
  data <- train
  data$split <- fold
  cl$split <- fold
  # vector of misclassification errors
  misclass_vec <- numeric(k_cv)
  # class predictions
  class <- vector(mode = "character", length = n)
  # iterating through different splits
  for (i in 1:k_cv) {
    # split train and test data
    data_train <- data %>% filter(split != i)
    data_test <- data %>% filter(split == i)
    # get appropriate class values
    cl_train <- cl %>% filter(split != i)
    cl_test <- cl %>% filter(split == i)
    # KNN
    knn_result <- knn(data_train[, names(data_train) != "split"],
                      data_test[, names(data_test) != "split"],
                      cl_train[, names(cl_train) != "split"],
                      k = k_nn, prob = TRUE)
    # misclassification error
    misclass_error <- sum(knn_result != cl_test[, 1]) / length(knn_result)
    misclass_vec[i] <- misclass_error
    # update class prediction values
    class[fold == i] <- as.character(knn_result)
  }
  # misclassification error
  cv_err <- mean(misclass_vec)
  # make outout list
  output <- list("class" = class, "cv_err" = cv_err)
  return(output)
}
