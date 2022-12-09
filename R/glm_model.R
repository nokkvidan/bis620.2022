#' @title glm_model
#' @param train training data
#' @param test testing data
#' @param y response variable
#' @param optimize how to optimize the glm
#' @importFrom MASS stepAIC
#' @importFrom predtools calibration_plot
#' @importFrom Metrics rmse mse
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc
#' @importFrom stats as.formula glm predict
#' @examples \dontrun{
#' glm_model(df, optimize = "manual")
#' glm_model(train, test, y, "stepAIC", evaluate = TRUE)
#' }
#' @export
glm_model <- function(y, train, test = NULL, optimize = NA) {
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
  if (optimize %!in% c(NA, "manual", "stepAIC")){
    stop("Please pick a valid optimizer: NA, `manual`, `stepAIC`")
  }
  # Begin the function
  form <- as.formula(paste0(y, "~", "."))
  fit <- glm(form, data = train)
  sum <- summary(fit)
  if (is.na(optimize)) {
    final_fit <- fit
  } else if (optimize == "manual") {
    sig <- which(sum$coef[, 4] < 0.1)[-1]
    sigvars <- rownames(sum$coefficients)[sig]
    form1 <- as.formula(paste("Diabetes_binary", "~",
                              paste(sigvars, collapse = "+")))
    fit1 <- glm(form1, data = train)
    sum1 <- summary(fit1)
    sig1 <- which(sum1$coef[, 4] < 0.05)[-1]
    sigvars1 <- rownames(sum1$coefficients)[sig1]
    form2 <- as.formula(paste("Diabetes_binary", "~",
                              paste(sigvars1, collapse = "+")))
    final_fit <- glm(form2, data = train)
  } else if (optimize == "stepAIC") {
    step <- MASS::stepAIC(fit)
    sigvars <- names(step$coefficients[-1])
    form2 <- as.formula(paste("Diabetes_binary", "~",
                              paste(sigvars, collapse = "+")))
    final_fit <- glm(form2, data = train)
  }
  if (!is.null(test)) {
    # Set up training results
    train_pred <- predict(final_fit, type = "response")
    train_pred_b <- ifelse(train_pred > 0.5, 1, 0)
    ev_train <- as.data.frame(cbind(y = train[, y],
                                    pred = train_pred,
                                    pred.b = train_pred_b))
    # Set up test results
    test_pred <- predict(final_fit, test, type = "response")
    test_pred_b <- ifelse(test_pred > 0.5, 1, 0)
    ev_test <- as.data.frame(cbind(y = test[, y],
                                   pred = test_pred,
                                   pred.b = test_pred_b))
    # Calibration plots
    p1 <- predtools::calibration_plot(data = ev_train, obs = "y", pred = "pred",
                           title = "Calibration plot for training data")
    p2 <- predtools::calibration_plot(data = ev_test, obs = "y", pred = "pred",
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
    ret <- list(final_fit, results, p1, p2, mses, confmat, test_roc)
  } else {
    ret <- final_fit
  }
  return(ret)
}
