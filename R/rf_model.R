#' @title rf_model
#' @param train training data
#' @param test test data
#' @param y response variable
#' @param optimize how to optimize the model
#' @param evaluate option to evaluate the model
#' @importFrom randomForest tuneRF randomForest
#' @importFrom predtools calibration_plot
#' @importFrom Metrics rmse mse
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc
#' @importFrom stats as.formula 
#' @examples \dontrun{
#' rf_model(train, test, y, optimize = TRUE, evaluate = TRUE)
#' }
#' @export
# Evaluate random forest models
rf_model <- function(train, y, test = NA, optimize = FALSE) {
  train[, y] <- as.factor(as.character(train[, y]))
  form <- as.formula(paste0(y, "~ ."))
  if (optimize ==  TRUE) {
    # Find the optimal mtry value
    mtry <- tuneRF(train[, !colnames(train) %in% y],
                   train[, colnames(train) %in% y],
                   ntreeTry = 100, stepFactor = 1.5,
                   improve = 0.01, trace = TRUE, plot = TRUE)
    best_m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
    rf <- randomForest(form, data = train, ntree = 100, norm.votes = FALSE,
                       do.trace = 10, mtry = best_m, importance = TRUE)
  } else {
    rf <- randomForest(form, data = train, ntree = 100, norm.votes = FALSE,
                       do.trace = 10, importance = TRUE)
  }
  if (!is.na(test)) {
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
    p1 <- calibration_plot(data = ev_train, obs = "y", pred = "pred",
                           title = "Calibration plot for training data")
    p2 <- calibration_plot(data = ev_test, obs = "y", pred = "pred",
                           title = "Calibration plot for validation data")
    # MSE and RMSE
    mse_train <- mse(actual = ev_train$y, predicted = ev_train$pred)
    rmse_train <- rmse(actual = ev_train$y, predicted = ev_train$pred)
    mse_test <- mse(actual = ev_test$y, predicted = ev_test$pred)
    rmse_test <- rmse(actual = ev_test$y, predicted = ev_test$pred)
    mses <- round(data.frame(MSE = c(mse_train, mse_test),
                             RMSE = c(rmse_train, rmse_test)), 3)
    row.names(mses) <- c("Training", "Validation")
    # Cross table and confusion matrix
    cross_table <- table(predicted = as.logical(ev_test$pred.b),
                         actual = as.logical(ev_test$y))
    confmat <- confusionMatrix(cross_table, positive = "TRUE")
    results <- list(ev_train, ev_test)
    # ROC
    test_roc <- roc(ev_test$y ~ ev_test$pred, plot = TRUE, print.auc = TRUE)
    ret <- list(rf, results, p1, p2, mses, confmat, test_roc)
  } else {
    ret <- rf
  }
  return(ret)
}
