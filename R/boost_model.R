#' @description Fits an XGBoost model to the input data and returns the model
#' object
#' @title boost_model
#' @param y character response variable
#' @param train training data that includes y
#' @param test test data that has the same column names as the training data, it
#' is an optional parameter with NULL default and triggers evaluation otherwise
#' @return if test is NULL, it returns a xgb.Booster object, if a test object is
#' added then a list of 6 objects. The first is a xgb.Booster object, the second
#' is a confusion matrix, the third is an importance matrix, the fourth is a
#' PRROC object for the ROC curve, the fifth is a PRROC object for a precision-
#' recall curve, the sixth is an roc object for the roc results
#' @importFrom xgboost xgb.DMatrix xgboost xgb.importance
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc
#' @importFrom PRROC pr.curve roc.curve
#' @importFrom stats predict
#' @examples \dontrun{
#' data(diabetes)
#' xg_fit <- boost_model("Diabetes_binary", diabetes)
#' }
#' @export
boost_model <- function(y, train, test = NULL) {
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
  ## Begin the function
  # Split the train data into dependent and independent variables
  x_train <- data.matrix(train[, !colnames(train) %in% y])
  y_train <- data.matrix(as.numeric(train[, colnames(train) %in% y]))
  # Convert the train and test data into xgboost matrix type.
  boost_train <- xgboost::xgb.DMatrix(data = x_train, label = y_train)
  # Train a model using our training data
  model <- xgboost::xgboost(data = boost_train, max.depth = 15, nrounds = 30)
  if (!is.null(test)) {
    # Convert the train and test data into xgboost matrix type.
    x_test <- data.matrix(test[, !colnames(test) %in% y])
    y_test <- data.matrix(as.numeric(test[, colnames(test) %in% y]))
    boost_test <- xgboost::xgb.DMatrix(data = x_test, label = y_test)
    # Use model to make predictions on test data and create confusion matrix
    pred_test <- predict(model, boost_test)
    pred_test_b <- as.factor(ifelse(pred_test > 0.5, 1, 0))
    y_test <- factor(y_test)
    pred_test <- as.numeric(pred_test)
    conf_mat <- caret::confusionMatrix(y_test, pred_test_b)
    # Compute feature importance matrix
    importance_matrix <- xgboost::xgb.importance(colnames(boost_train),
                                                 model = model)
    # ROC and PRAUC
    score1 <- pred_test[y_test == 1]
    score0 <- pred_test[y_test == 0]
    roc <- PRROC::roc.curve(score1, score0, curve = TRUE)
    pr <- PRROC::pr.curve(score1, score0, curve = TRUE)
    # ROC test
    roc_test <- pROC::roc(y_test, pred_test, algorithm = 2, plot = TRUE,
                          print.auc = TRUE)
    # Combine them together in a list to a return variable
    ret <- list(model, conf_mat, importance_matrix, roc, pr, roc_test)
  } else {
    # If no test data, then just assign the model to the return variable
    ret <- model
  }
  return(ret)
}
