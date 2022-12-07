#' @title boost_model
#' @param train training data
#' @param test test data
#' @param y response variable
#' @importFrom xgboost xgb.DMatrix xgboost xgb.importance
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc
#' @importFrom PRROC pr.curve roc.curve
#' @importFrom stats predict
#' @examples \dontrun{
#' xg.model <- boost_model(y, train, test)
#' plot(xg.model[[5]], main="Out-Of-sample PR curve")
#' xgb.plot.importance(xg.model[[3]][1:20,])
#' }
#' @export
boost_model <- function(y, train, test = NULL) {
  x_train <- data.matrix(train[, !colnames(train) %in% y])
  y_train <- data.matrix(as.numeric(train[, colnames(train) %in% y]))
  # convert the train and test data into xgboost matrix type.
  boost_train <- xgb.DMatrix(data = x_train, label = y_train)
  # train a model using our training data
  model <- xgboost(data = boost_train, max.depth = 15, nrounds = 30)
  if (!is.null(test)) {
    # convert the train and test data into xgboost matrix type.
    x_test <- data.matrix(test[, !colnames(test) %in% y])
    y_test <- data.matrix(as.numeric(test[, colnames(test) %in% y]))
    boost_test <- xgb.DMatrix(data = x_test, label = y_test)
    # use model to make predictions on test data
    pred_test <- predict(model, boost_test)
    pred_test_b <- as.factor(ifelse(pred_test > 0.5, 1, 0))
    y_test <- factor(y_test)
    pred_test <- as.numeric(pred_test)
    conf_mat <- confusionMatrix(y_test, pred_test_b)
    # Compute feature importance matrix
    importance_matrix <- xgb.importance(colnames(boost_train), model = model)
    roc_test <- roc(y_test, pred_test, algorithm = 2, plot = TRUE,
                    print.auc = TRUE)
    # ROC and PRAUC
    score1 <- pred_test[y_test == 1]
    score0 <- pred_test[y_test == 0]
    roc <- roc.curve(score1, score0, curve = TRUE)
    pr <- pr.curve(score1, score0, curve = TRUE)
    ret <- list(model, conf_mat, importance_matrix, roc, pr, roc_test)
  } else {
    ret <- model
  }
  return(ret)
}
