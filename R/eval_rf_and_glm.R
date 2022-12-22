#' @description Run random forest or GLM model
#' @title eval_rf_and_glm
#' @param train training data that includes y
#' @param test test data that has the same column names as the training data, it
#' is an optional parameter with NULL default and triggers evaluation otherwise
#' @param model random forest or GLM to evaluate
#' @param type character type of data to predict ("prob" for random forest, and
#' response" for glm)
#' @return data.frame counterfactual table
#' @importFrom stats predict
#' @examples \dontrun{
#' data(diabetes)
#' rf_fit <- rf_model("Diabetes_binary", train, test, optimize = TRUE)
#' eval_rf_and_glm(rf_fit, type = "prob")
#'
#' glm_fit <- glm_model("Diabetes_binary", train, test, optimize = "manual")
#' eval_rf_and_glm(glm_fit, type = "response")
#' }
#' @export
eval_rf_and_glm <- function(train, test, model, type = "response") {
    df <- data.frame(matrix(nrow = 2, ncol = 4))
    colnames(df) <- c("BMI", "HighBP", "HighChol", "HeartDiseaseorAttack")
    rownames(df) <- c("before", "after")

    ## BMI

    subset <- test[test$Diabetes_binary == 1, ]
    pred_test <- predict(model[[1]], subset, type)
    pred_test_b_before <- ifelse(pred_test > 0.5, 1, 0)
    df[1, "BMI"] <- mean(pred_test_b_before)
    subset[subset$BMI > 17, "BMI"] <- 17
    subset[subset$BMI < 15, "BMI"] <- 15
    pred_test <- predict(model[[1]], subset, type)
    pred_test_b_after <- ifelse(pred_test > 0.5, 1, 0)
    df[2, "BMI"] <- mean(pred_test_b_after)

    ## HeartDiseaseorAttack
    subset <- test[test$Diabetes_binary == 1 & test$HeartDiseaseorAttack == 1, ]
    pred_test <- predict(model[[1]], subset, type)
    pred_test_b_before <- ifelse(pred_test > 0.5, 1, 0)
    df[1, "HeartDiseaseorAttack"] <- mean(pred_test_b_before)
    subset[, "HeartDiseaseorAttack"] <- sapply(
      subset[, "HeartDiseaseorAttack"], replace, factor(1), factor(0))
    pred_test <- predict(model[[1]], subset, type)
    pred_test_b_after <- ifelse(pred_test > 0.5, 1, 0)
    df[2, "HeartDiseaseorAttack"] <- mean(pred_test_b_after)

    ## HighBP

    subset <- test[test$Diabetes_binary == 1 & test$HighBP == 1, ]
    pred_test <- predict(model[[1]], subset, type)
    pred_test_b_before <- ifelse(pred_test > 0.5, 1, 0)
    df[1, "HighBP"] <- mean(pred_test_b_before)
    subset[, "HighBP"] <- sapply(subset[, "HighBP"], replace,
                                       factor(1), factor(0))
    pred_test <- predict(model[[1]], subset, type)
    pred_test_b_after <- ifelse(pred_test > 0.5, 1, 0)
    df[2, "HighBP"] <- mean(pred_test_b_after)

    ## HighChol

    subset <- test[test$Diabetes_binary == 1 & test$HighChol == 1, ]
    pred_test <- predict(model[[1]], subset, type)
    pred_test_b_before <- ifelse(pred_test > 0.5, 1, 0)
    df[1, "HighChol"] <- mean(pred_test_b_before)
    subset[, "HighChol"] <- sapply(subset[, "HighChol"], replace,
                                       factor(1), factor(0))
    pred_test <- predict(model[[1]], subset, type)
    pred_test_b_after <- ifelse(pred_test > 0.5, 1, 0)
    df[2, "HighChol"] <- mean(pred_test_b_after)

    df <- as.data.frame(t(df))
    df <- cbind(df, df[, 2] - df[, 1])
    colnames(df) <- c("Before", "After", "Change")
    return(df)
}
