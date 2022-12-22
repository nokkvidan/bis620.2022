#' @description Run XGBoost model
#' @title eval_glm
#' @param train training data that includes y
#' @param test test data that has the same column names as the training data, it
#' is an optional parameter with NULL default and triggers evaluation otherwise
#' @param model function XGBoost model to evaluate
#' @param y_name character name of response variable in data set
#' @return data.frame counterfactual table
#' @importFrom Matrix sparse.model.matrix
#' @examples \dontrun{
#' data(diabetes)
#' rf_fit <- rf_model("Diabetes_binary", train, test, optimize = TRUE)
#' eval_rf_and_glm(rf_fit, type = "prob")
#'
#' glm_fit <- glm_model("Diabetes_binary", train, test, optimize = "manual")
#' eval_rf_and_glm(glm_fit, type = "response")
#' }
#' @export
eval_xg <- function(train, test, model, y_name = "Diabetes_binary")  {
    before <- c()
    after <- c()

    ### BMI

    subset <- test[test[, y_name] == 1, ]
    form <- as.formula(paste0(y_name, " ~ ."))
    testm <- Matrix::sparse.model.matrix(form, data = subset)[, -1]
    pred_test <- predict(model[[1]], newdata = testm, type = "prob")
    pred_test_b <- ifelse(pred_test > 0.5, 1, 0)
    before <- c(before, round(mean(pred_test_b), 4))

    subset[subset$"BMI" > 17, "BMI"] <- 17
    subset[subset$"BMI" < 15, "BMI"] <- 15
    testm <- Matrix::sparse.model.matrix(form, data = subset)[, -1]
    pred_test <- predict(model[[1]], newdata = testm,  type = "prob")
    pred_test_b <- ifelse(pred_test > 0.5, 1, 0)
    after <- c(after, round(mean(pred_test_b), 4))

    ### HeartDiseaseorAttack

    subset <- test[test$Diabetes_binary == 1 & test$HeartDiseaseorAttack == 1, ]
    form <- as.formula(paste0(y_name, " ~ ."))
    testm <- Matrix::sparse.model.matrix(form, data = subset)[, -1]
    pred_test <- predict(model[[1]], newdata = testm, type = "prob")
    pred_test_b <- ifelse(pred_test > 0.5, 1, 0)
    before <- c(before, round(mean(pred_test_b), 4))

    subset$HeartDiseaseorAttack <- sapply(subset[, "HeartDiseaseorAttack"],
                                          replace, factor(1), factor(0))
    testm <- Matrix::sparse.model.matrix(form, data = subset)[, -1]
    pred_test <- predict(model[[1]], newdata = testm,  type = "prob")
    pred_test_b <- ifelse(pred_test > 0.5, 1, 0)
    after <- c(after, round(mean(pred_test_b), 4))

    ### HighBP

    subset <- test[test$Diabetes_binary == 1 & test$HighBP == 1, ]
    form <- as.formula(paste0(y_name, " ~ ."))
    testm <- Matrix::sparse.model.matrix(form, data = subset)[, -1]
    pred_test <- predict(model[[1]], newdata = testm, type = "prob")
    pred_test_b <- ifelse(pred_test > 0.5, 1, 0)
    before <- c(before, round(mean(pred_test_b), 4))

    subset$HighBP <- sapply(subset[, "HighBP"], replace, factor(1), factor(0))
    testm <- Matrix::sparse.model.matrix(form, data = subset)[, -1]
    pred_test <- predict(model[[1]], newdata = testm,  type = "prob")
    pred_test_b <- ifelse(pred_test > 0.5, 1, 0)
    after <- c(after, round(mean(pred_test_b), 4))

    ### HighChol

    subset <- test[test$Diabetes_binary == 1 & test$HighChol == 1, ]
    form <- as.formula(paste0(y_name, " ~ ."))
    testm <- Matrix::sparse.model.matrix(form, data = subset)[, -1]
    pred_test <- predict(model[[1]], newdata = testm, type = "prob")
    pred_test_b <- ifelse(pred_test > 0.5, 1, 0)
    before <- c(before, round(mean(pred_test_b), 4))

    subset$HighChol <- sapply(subset[, "HighChol"], replace, factor(1),
                              factor(0))
    testm <- Matrix::sparse.model.matrix(form, data = subset)[, -1]
    pred_test <- predict(model[[1]], newdata = testm,  type = "prob")
    pred_test_b <- ifelse(pred_test > 0.5, 1, 0)
    after <- c(after, round(mean(pred_test_b), 4))

    ### Collect results
    change <- round(after - before, 4)
    df <- data.frame(Before = before, After = after, Change = change)
    rownames(df) <- c("BMI", "HeartDiseaseorAttack", "HighBP", "HighChol")
    return(df)
}
