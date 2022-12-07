
#df <- read.csv("~/Desktop/diabetes_binary.csv")
#df <- read.csv("/Users/nokkvi/Desktop/Yale/F22/F22_BIS620/bis620.2022/data/diabetes_binary_5050split_health_indicators_BRFSS2015.csv")


library(ggplot2)

#' @param d diabetes data frame
#' @param y an arbitrary variable in diabetes data frame
#' @param x an arbitrary variable in diabetes data frame
#' @importFrom ggplot2 aes_string ggplot geom_line facet_grid aes
#' @example \dontrun{
#' vis_2vars(df, 'Diabetes_binary', 'BMI')
#' }
vis_2vars <- function(d, y, x) {
  col1 <- d[, x]
  col2 <- d[, y]
  fac1 <- length(unique(col1)) <= 15
  fac2 <- length(unique(col2)) <= 15
  if (fac1 && fac2) {
    p <- ggplot(d, aes(x = eval(str2lang(x)), fill = eval(str2lang(y)))) +
      geom_bar(position = "dodge")
    subtitle <- "Barplot visulization"
  } else if (fac1 && !fac2) {
    p <- ggplot(d) +
      geom_violin(aes(x = eval(str2lang(x)), y = eval(str2lang(y))))
    subtitle <- "Violinplot visulization"
  } else if (!fac1 && fac2) {
    p <- ggplot(d) +
      geom_violin(aes(x = eval(str2lang(x)), y = eval(str2lang(x))))
    subtitle <- "Violinplot visulization"
  } else if (!fac1 && !fac2) {
    p <- ggplot(d) +
      geom_point(aes(x = eval(str2lang(x)), y = eval(str2lang(y))))
    subtitle <- "Scatterplot visulization"
  }
  title <- paste0("Visulizing the variables: ", x, " & ", y)
  p <- p + labs(title = title, subtitle = subtitle, x = x, y = y) + theme_bw()
  return(p)
}

d <- df
x <- "Diabetes_binary"
y <- "BMI"
vis.2vars(d, x, y)


#' @param d diabetes data frame
#' @importFrom ggplot2 aes_string ggplot geom_boxplot facet_grid aes theme_bw
#' @importFrom gridExtra grid.arrange
#' @example \dontrun{
#' vis.num(df)
#' }
vis.num <- function(d) {
  d_num <- d[, sapply(d, is.numeric)]
  d_num_scaled <- data.frame(
    apply(d_num, 2, function(x) (x - min(x)) / (max(x) - min(x))))
  p1 <- ggplot(stack(d_num), aes(x = ind, y = values)) +
    geom_boxplot() +
    labs(title = "Visulizing Numeric Variables",
         subtitle = "Boxplot visulization",
         x = "Values",
         y = "Variables") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  p2 <- ggplot(stack(d_num_scaled), aes(x = ind, y = values)) +
    geom_boxplot() +
    labs(title = "Visulizing Scaled Numeric Variables",
         subtitle = "Boxplot visulization",
         x = "Scaled Values",
         y = "Variables") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  return(gridExtra::grid.arrange(p1, p2, ncol = 2))
}

vis.num(df)


#' @param d diabetes data frame
#' @importFrom car qqPlot
#' @example \dontrun{
#' vis_dist(df)
#' }
vis_dist <- function(d) {
  df_num <- df[, sapply(df, is.numeric)]
  par(mfrow = c(2, 2))
  # Plotting for loop
  for (i in seq_len(ncol(df_num))) {
    hist(df_num[, i], col = "red", pch = 19, xlab = names(df_num)[i],
         main = paste("Histogram of ", names(df_num)[i], sep = ""))
    qqPlot(df_num[, i], col = "red", pch = 19, ylab = names(df_num)[i],
           main = paste("QQ-Plot for ", names(df_num)[i], sep = ""))
  }
}

library(predtools)
library(Metrics)
#' @param train training data
#' @param test testing data
#' @param optimize how to optimize the glm
#' @param evaluate option to evaluate the model
#' @importFrom MASS stepAIC
#' @importFrom predtools calibration_plot
#' @importFrom Metrics rmse mse
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc
#' @example \dontrun{
#' glm_model(df, optimize = "manual")
#' glm_model(train, test, y, "stepAIC", evaluate = TRUE)
#' }
glm_model <- function(train, test, y, optimize = NA, evaluate = FALSE) {
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
  if (evaluate == TRUE) {
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
    ret <- list(final_fit, results, p1, p2, mses, confmat, test_roc)
  } else {
    ret <- final_fit
  }
  return(ret)
}


################################################################################

library(car)
library(xgboost)
library(caret)

library(pROC)
library(PRROC)
#' @param train training data
#' @param test test data
#' @param y response variable
#' @param evaluate option to evaluate the model
#' @importFrom xgboost xgb.DMatrix xgboost xgb.importance
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc
#' @importFrom PRROC pr.curve roc.curve
#' @example \dontrun{
#' xg.model <- boost_model(train, test, y, evaluate = TRUE)
#' plot(xg.model[[5]], main="Out-Of-sample PR curve")
#' xgb.plot.importance(xg.model[[3]][1:20,])
#' }
boost_model <- function(train, test, y, evaluate = FALSE) {
  x_train <- data.matrix(train[, !colnames(train) %in% y])
  y_train <- data.matrix(as.numeric(train[, colnames(train) %in% y]))
  x_test <- data.matrix(test[, !colnames(test) %in% y])
  y_test <- data.matrix(as.numeric(test[, colnames(test) %in% y]))
  # convert the train and test data into xgboost matrix type.
  boost_train <- xgb.DMatrix(data = x_train, label = y_train)
  boost_test <- xgb.DMatrix(data = x_test, label = y_test)
  # train a model using our training data
  model <- xgboost(data = boost_train, max.depth = 15, nrounds = 30)
  if (evaluate == TRUE) {
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



library(randomForest)
#' @param train training data
#' @param test test data
#' @param y response variable
#' @param evaluate option to evaluate the model
#' @importFrom randomForest tuneRF randomForest
#' @importFrom predtools calibration_plot
#' @importFrom Metrics rmse mse
#' @importFrom caret confusionMatrix
#' @importFrom pROC roc
#' @example \dontrun{
#' rf_model(train, test, y, optimize = TRUE, evaluate = TRUE)
#' }
# Evaluate random forest models
rf_model <- function(train, test, y, optimize = FALSE, evaluate = FALSE) {
  train[, y] <- as.factor(as.character(train[, y]))
  form <- as.formula(paste0(y, "~", "."))
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
  if (evaluate == TRUE) {
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
