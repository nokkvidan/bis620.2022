#' @title rf_model
#' @param train training data
#' @param test test data
#' @param y response variable
#' @param optimize how to optimize the model
#' @importFrom randomForest tuneRF randomForest
#' @importFrom predtools calibration_plot
#' @importFrom Metrics rmse mse
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc
#' @importFrom stats as.formula
#' @examples \dontrun{
#' rf_model("Diabetes_binary", train, test, "stepAIC")
#' }
#' @export
# Evaluate random forest models
rf_model <- function(y, train, test = NULL, optimize = FALSE) {
  # Check parameters
  if (y %!in% colnames(train)) {
    stop(paste0(y, " is not a variable in your train data"))
  }
  if (!is.null(test)){
    cols <- length(setdiff(colnames(train), colnames(test)))
    if(cols != 0){
      stop("The column titles of your train and test sets do not match")
    }
  }
  if (optimize %!in% c(TRUE, FALSE)){
    stop("Please pick a valid optimizer: TRUE or FALSE")
  }
  # Begin the function
  train[, y] <- as.factor(as.character(train[, y]))
  form <- as.formula(paste0(y, "~ ."))
  if (optimize ==  TRUE) {
    # Find the optimal mtry value
    mtry <- randomForest::tuneRF(train[, !colnames(train) %in% y],
                                 train[, colnames(train) %in% y],
                                 ntreeTry = 100, stepFactor = 1.5,
                                 improve = 0.01, trace = TRUE, plot = TRUE)
    best_m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
    rf <- randomForest::randomForest(form, data = train, ntree = 100,
                                     norm.votes = FALSE, do.trace = 10,
                                     mtry = best_m, importance = TRUE)
  } else {
    rf <- randomForest::randomForest(form, data = train,
                                     ntree = 100, norm.votes = FALSE,
                                     do.trace = 10, importance = TRUE)
  }
  if (!is.null(test)) {
    pred_train <- predict(rf, type = "prob")
    pred_test <- predict(rf, newdata = test, type = "prob")
    train_pred_b <- as.factor(ifelse(pred_train[, 1] < 0.5, TRUE, FALSE))
    test_pred_b <- as.factor(ifelse(pred_test[, 1] < 0.5, TRUE, FALSE))
    ev_train <- data.frame(y = as.numeric(as.logical(train[, y])),
                           pred = as.numeric(pred_train[, 2]),
                           pred.b = train_pred_b)
    ev_test <- data.frame(y = as.numeric(test[, y]),
                          pred = as.numeric(pred_test[, 2]),
                          pred.b = test_pred_b)
    # Calibration plots
    p1 <- predtools::calibration_plot(
      data = ev_train, obs = "y", pred = "pred",
      title = "Calibration plot for training data")
    p2 <- predtools::calibration_plot(
      data = ev_test, obs = "y", pred = "pred",
      title = "Calibration plot for validation data")
    # MSE and RMSE
    mse_train <- Metrics::mse(actual = ev_train$y, predicted = ev_train$pred)
    rmse_train <- Metrics::rmse(actual = ev_train$y, predicted = ev_train$pred)
    mse_test <- Metrics::mse(actual = ev_test$y, predicted = ev_test$pred)
    rmse_test <- Metrics::rmse(actual = ev_test$y, predicted = ev_test$pred)
    mses <- round(data.frame(MSE = c(mse_train, mse_test),
                             RMSE = c(rmse_train, rmse_test)), 3)
    row.names(mses) <- c("Training", "Validation")
    # Cross table and confusion matrix
    cross_table <- table(predicted = as.logical(ev_test$pred.b),
                         actual = as.logical(ev_test$y))
    confmat <- caret::confusionMatrix(cross_table, positive = "TRUE")
    results <- list(ev_train, ev_test)
    # ROC
    test_roc <- pROC::roc(ev_test$y ~ ev_test$pred, plot = TRUE,
                          print.auc = TRUE)
    ret <- list(rf, results, p1, p2, mses, confmat, test_roc)
  } else {
    ret <- rf
  }
  return(ret)
}
