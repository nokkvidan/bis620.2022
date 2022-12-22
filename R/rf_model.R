#' @description  Fits a random forest model to the input data and returns the
#' model object
#' @title rf_model
#' @param y character response variable
#' @param train training data that includes y
#' @param test test data that has the same column names as the training data, it
#' is an optional parameter with NULL default and triggers evaluation otherwise
#' @param optimize a boolean to optimize the model with default FALSE. If the
#' parameter is set to TRUE, then the function uses randomForest::tuneRF to find
#' the best mtry and then runs the random forest using that value, otherwise it
#' runs the random forest without the default mtry.
#' @return If there is no test data, then the function returns a randomForest
#' object. If there is test data, then the function returns a list with 7 items.
#' The first is a randomForst object, the second is a list of the training and
#' testing prediction the third is a calibration plot for the training data, the
#' fourth is a calibration plot for the validation data, the fifth is a data
#' frame with the train and test MSE and the train and test RMSE, the sixth is a
#' confusion matrix, the seventh is a pROC opject for the test ROC
#' @importFrom randomForest tuneRF randomForest
#' @importFrom predtools calibration_plot
#' @importFrom Metrics rmse mse
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc
#' @importFrom stats as.formula
#' @examples \dontrun{
#' data(diabetes)
#' rf_fit <- rf_model("Diabetes_binary", diabetes, optimize = TRUE)
#' }
#' @export
# Evaluate random forest models
rf_model <- function(y, train, test = NULL, optimize = FALSE) {
  ## Check parameters
  # Check that the given y variable exists in the data
  if (y %nin% colnames(train)) {
    stop(paste0(y, " is not a variable in your train data"))
  }

  # If there is test data, check that their columns match
  if (!is.null(test)) {
    diff1 <- names(train) %nin% names(test)
    diff2 <- names(test) %nin% names(train)
    if (sum(diff1, diff2) > 0) {
      stop("The column titles of your train and test sets do not match")
    }
  }
  # Check that the given optimize value is a valid one
  if (optimize %nin% c(TRUE, FALSE)) {
    stop("Please pick a valid optimizer: TRUE or FALSE")
  }
  ## Begin the function
  # Create the formula with the training data
  train[, y] <- as.factor(as.character(train[, y]))
  form <- as.formula(paste0(y, "~ ."))
  # If optimize is TRUE, then:
  if (optimize ==  TRUE) {
    # Tune the random forest and find the optimal mtry value
    mtry <- randomForest::tuneRF(x = train[, !colnames(train) %in% y],
                                 y = train[, colnames(train) %in% y],
                                 ntreeTry = 100, stepFactor = 1.5,
                                 improve = 0.01, trace = TRUE, plot = TRUE)
    best_m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
    # Rerun the random forest with that value
    rf <- randomForest::randomForest(form, data = train, ntree = 100,
                                     norm.votes = FALSE, do.trace = 10,
                                     mtry = best_m, importance = TRUE)
  } else {
    # If optimize is FALSE, then run the random forest as normal
    rf <- randomForest::randomForest(form, data = train,
                                     ntree = 100, norm.votes = FALSE,
                                     do.trace = 10, importance = TRUE)
  }
  # If testing data is provided:
  if (!is.null(test)) {
    # Use the generated random forest and run it on both the train and test data
    # to get predictions
    pred_train <- predict(rf, type = "prob")
    pred_test <- predict(rf, newdata = test, type = "prob")
    train_pred_b <- as.factor(ifelse(pred_train[, 1] > 0.5, TRUE, FALSE))
    test_pred_b <- as.factor(ifelse(pred_test[, 1] > 0.5, TRUE, FALSE))
    # Create a data frame of the training and the testing evaluation
    ev_train <- data.frame(y = as.numeric(as.logical(train[, y])),
                           pred = as.numeric(pred_train[, 2]),
                           pred.b = train_pred_b)
    ev_test <- data.frame(y = as.numeric(test[, y]),
                          pred = as.numeric(pred_test[, 2]),
                          pred.b = test_pred_b)
    results <- list(ev_train, ev_test)
    # Create calibration plots for both the training and testing evaluation
    # prediction data frames
    p1 <- predtools::calibration_plot(
      data = ev_train, obs = "y", pred = "pred",
      title = "Calibration plot for training data")
    p2 <- predtools::calibration_plot(
      data = ev_test, obs = "y", pred = "pred",
      title = "Calibration plot for validation data")

    # MSE and RMSE for both training and testing predictions and make data frame
    mse_train <- Metrics::mse(actual = ev_train$y, predicted = ev_train$pred)
    rmse_train <- Metrics::rmse(actual = ev_train$y, predicted = ev_train$pred)
    mse_test <- Metrics::mse(actual = ev_test$y, predicted = ev_test$pred)
    rmse_test <- Metrics::rmse(actual = ev_test$y, predicted = ev_test$pred)
    mses <- round(data.frame(MSE = c(mse_train, mse_test),
                             RMSE = c(rmse_train, rmse_test)), 3)
    row.names(mses) <- c("Training", "Validation")
    # Create cross table and confusion matrix and add to a list
    cross_table <- table(predicted = as.logical(ev_test$pred.b),
                         actual = ifelse(ev_test$y == 1, TRUE, FALSE))
    confmat <- caret::confusionMatrix(cross_table, positive = "TRUE")
    # ROC for test data
    test_roc <- pROC::roc(ev_test$y ~ ev_test$pred, plot = TRUE,
                          print.auc = TRUE)
    # Add the objects to a list to return
    ret <- list(rf, results, p1, p2, mses, confmat, test_roc)
  } else {
    # If there is no test data, then just return the random forest result
    ret <- rf
  }
  return(ret)
}
