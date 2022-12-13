#' @description Fits a GLM to the input data and returns the model object
#' @title glm_model
#' @param y character response variable
#' @param train training data that includes y
#' @param test test data that has the same column names as the training data, it
#' is an optional parameter with NULL default and triggers evaluation otherwise
#' @param optimize how to optimize the glm with default NA, meaning the function
#' does not run with optimization. The other options are "manual" and "stepAIC".
#' If manual is selected, the model will remove variables with less than 0.1
#' significance, rerun the model, and then remove variables with less than 0.05
#' significance for the final model. If stepAIC is selected, then the model will
#' use the MASS::stepAIC function to check which coefficients are significant,
#' remove them, rerun the glm for the final model
#' @return if test is NULL, then the function returns a glm object. If the user
#' inputs a test, then the function outputs a list of size 7. The first item is
#' a glm object, the second is a list of the training and testing prediction,
#' the third is a calibration plot for the training data, the fourth is a
#' calibration plot for the validation data, the fifth is a data frame with the
#' train and test MSE and the train and test RMSE, the sixth is a confusion
#' matrix, the seventh is a pROC opject for the test ROC
#' @importFrom MASS stepAIC
#' @importFrom predtools calibration_plot
#' @importFrom Metrics rmse mse
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc
#' @importFrom stats as.formula glm predict
#' @examples \dontrun{
#' data(diabetes)
#' glm_fit <- glm_model("Diabetes_binary", diabetes, optimize = 'manual')
#' }
#' @export
glm_model <- function(y, train, test = NULL, optimize = NA) {
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
  if (optimize %nin% c(NA, "manual", "stepAIC")) {
    stop("Please pick a valid optimizer: NA, `manual`, `stepAIC`")
  }
  ## Begin the function
  # Create the glm formula and run it
  form <- as.formula(paste0(y, "~", "."))
  fit <- glm(form, data = train, family = binomial)
  # Save the summary for optimization use
  sum <- summary(fit)
  # If there is no optimization, then the glm fit is the final one
  if (is.na(optimize)) {
    final_fit <- fit
  } else if (optimize == "manual") {
    # For manual optimization start with removing any values with less than 0.1
    # significance
    sig <- which(sum$coef[, 4] < 0.1)[-1]
    sigvars <- rownames(sum$coefficients)[sig]
    # Then re run the model on significant variables only
    form1 <- as.formula(paste("Diabetes_binary", "~",
                              paste(sigvars, collapse = "+")))
    fit1 <- glm(form1, data = train, family = binomial)
    # Save this new model's summary and then rerun with 0.05 significance
    sum1 <- summary(fit1)
    sig1 <- which(sum1$coef[, 4] < 0.05)[-1]
    sigvars1 <- rownames(sum1$coefficients)[sig1]
    form2 <- as.formula(paste("Diabetes_binary", "~",
                              paste(sigvars1, collapse = "+")))
    # This is now the final model
    final_fit <- glm(form2, data = train, family = binomial)
  } else if (optimize == "stepAIC") {
    # For stepAIC optimization, then fit the stepAIC algorithm to remove coeffs
    step <- MASS::stepAIC(fit)
    sigvars <- names(step$coefficients[-1])
    # Rerun the glm
    form2 <- as.formula(paste("Diabetes_binary", "~",
                              paste(sigvars, collapse = "+")))
    final_fit <- glm(form2, data = train, family = binomial)
  }
  # If testing data is provided:
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
    # Train and test predictions
    results <- list(ev_train, ev_test)
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
    # ROC
    test_roc <- pROC::roc(ev_test$y ~ ev_test$pred, plot = TRUE,
                          print.auc = TRUE)
    ret <- list(final_fit, results, p1, p2, mses, confmat, test_roc)
  } else {
    ret <- final_fit
  }
  return(ret)
}
