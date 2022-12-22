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
#' @importFrom Matrix sparse.model.matrix
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
  form <- as.formula(paste0(y, " ~ ."))
  sparse_train <- Matrix::sparse.model.matrix(form, data = train)[, -1]
  output_vector <- train[, y] == 1
  model <- xgboost::xgboost(data = sparse_train, label = output_vector,
                            max_depth = 15, eta = 1, nthread = 2, nrounds = 30,
                            objective = "binary:logistic")

  # Train a model using our training data
  if (!is.null(test)) {
    sparse_test <- Matrix::sparse.model.matrix(form, data = test)[, -1]
    pred_test <- predict(model, sparse_test)
    pred_test_b <- as.logical(ifelse(pred_test > 0.5, 1, 0))
    y_test <- test[, y]
    y_test_b <- ifelse(y_test == 1, TRUE, FALSE)
    cross_table <- table(predicted = pred_test_b, actual = y_test_b)
    confmat <- caret::confusionMatrix(cross_table, positive = "TRUE")

    # Compute feature importance matrix
    importance_matrix <- xgboost::xgb.importance(colnames(sparse_train),
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
    ret <- list(model, confmat, importance_matrix, roc, pr, roc_test)
  } else {
    # If no test data, then just assign the model to the return variable
    ret <- model
  }
  return(ret)
}
