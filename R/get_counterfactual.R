#' @description Computes counterfactual table for diabetes data. A
#' counterfactual table compares how changes to certain variables affects the
#' predicted proportion of diabetes among the diabetic population. Specifically,
#' we are interested in the variables: `BMI`, `HeartDiseaseorAttack`, `HighBP`,
#' `HighChol`, and `PhysActivity` variables. We adjust BMI to a range of 15-17,
#' which falls within the CDC's 5-85 percentile for ages 13 and under. The
#' latter 4 variables are binary categorical variables, and we adjust them from
#' unhealthy to healthy values, e.g., `PhysActivity == 0` (no regular
#' physical activity) to `PhysActivity == 1`.
#' @title get_counterfactual
#' @param model fitted model for predictions (must be one of random forest, glm,
#' or XGBoost)
#' @param data diabetes data frame
#' @return counterfactual dataframe, where rows are the variables of interest,
#' and columns are the predicted diabetic proportion before/after changing the
#' corresponding variable (note that the `Before` column is equivalent to the
#' model's accuracy because the subsets are from the diabetic population only)
#' @importFrom purrr map2 map_dbl
#' @importFrom Matrix sparse.model.matrix
#' @examples \dontrun{
#' data(diabetes)
#' rf_fit <- rf_model("Diabetes_binary", diabetes, optimize = TRUE)
#' counterfactual <- get_counterfactual(rf_fit[[1]], diabetes)
#' }
#' @export
get_counterfactual <- function(model, data) {

  binary_factors <- c("HeartDiseaseorAttack", "HighBP", "HighChol",
                      "PhysActivity")
  binary_levels <- c(1, 1, 1, 0)  # unhealthy levels

  # nolint start
  # helper function to predict diabetic proportion
  predict_diabetic_proportion <- function(model, data) {
    if (inherits(model, "glm")) {
      preds <- predict(model, data, type = "response")
    } else if (inherits(model, "randomForest")) {
      preds <- predict(model, data, type = "prob")[, "1"]
    } else if (inherits(model, "xgb.Booster")) {
      matrix_formula <- as.formula("Diabetes_binary ~ .")
      data <- Matrix::sparse.model.matrix(matrix_formula, data)[, -1]
      preds <- predict(model, data, type = "prob")
    } else {
      stop("Model must inherit from randomForest, xgb.Booster, or glm")
    }
    return(mean(preds > 0.5))
  }
  # nolint end

  # get proportion of diabetic population, subset with unhealthy variable levels
  diabetic <- subset(data, get("Diabetes_binary") == 1)
  binary_subsets <- purrr::map2(binary_factors, binary_levels,
                                ~ subset(diabetic, get(.x) == .y))
  bmi_subset <- subset(diabetic, (get("BMI") < 15) | (get("BMI") > 17))
  dataframes <- append(list(bmi_subset), binary_subsets)
  before <- purrr::map_dbl(dataframes, ~ predict_diabetic_proportion(model, .x))

  # get proportion of diabetic population, subset with healthy variable levels
  # change to healthy levels for binary factors
  binary_subsets <- purrr::map(seq_along(binary_factors), function(i) {
    var <- binary_factors[i]
    old_level <- factor(binary_levels[i])
    new_level <- factor(0 + !binary_levels[i])
    df <- binary_subsets[[i]]
    df[, var] <- sapply(df[, var], replace, old_level, new_level)
    return(df)
  })

  # change to healthy BMI range
  bmi_subset[, "BMI"] <- purrr::map_dbl(bmi_subset[, "BMI"],
                                        ~ if (.x < 15) return(15)
                                        else if (.x > 17) return(17)
                                        else return(.x))
  dataframes <- append(list(bmi_subset), binary_subsets)
  after <- purrr::map_dbl(dataframes, ~ predict_diabetic_proportion(model, .x))

  # build counterfactual table
  counterfactual <- data.frame(Before = before,
                               After = after,
                               Change = after - before)
  rownames(counterfactual) <- c("BMI", binary_factors)
  return(counterfactual)
}
