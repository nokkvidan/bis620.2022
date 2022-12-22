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
#' counterfactual <- get_counterfactual(rf_fit[[1]], diabetes, TRUE)
#' }
#' @export
get_counterfactual <- function(model, data) {

  binary_factors <- c("HeartDiseaseorAttack", "HighBP", "HighChol",
                      "PhysActivity")
  levels <- c(1, 1, 1, 0)  # starting unhealthy levels

  # nolint start
  # helper function to predict diabetic proportion
  predict_diabetic_proportion <- function(model, data) {
    if (inherits(model, "glm")) {
      type <- "response"
    } else if (inherits(model, "randomForest")) {
      type <- "prob"
    } else if (inherits(model, "xgb.Booster")) {
      matrix_formula <- as.formula("Diabetes_binary ~ .")
      data <- Matrix::sparse.model.matrix(matrix_formula, data)[, -1]
      type <- "prob"
    } else {
      stop("Model must inherit from randomForest, xgb.Booster, or glm")
    }
    return(mean(predict(model, data, type) > 0.5))
  }
  # nolint end

  # get proportion of diabetic population, subset with each binary factor
  subsets <- purrr::map2(binary_factors, levels, ~subset(data, get(.x) == .y),
                         subset(data, get("Diabetes_binary") == 1))
  dataframes <- append(list(data), subsets)
  before <- purrr::map_dbl(dataframes, ~ predict_diabetic_proportion(model, .x))

  # get proportion of diabetic population, subset with opposite level of
  # each binary factor
  levels <- purrr::map_int(levels, ~ !.x)
  subsets <- purrr::map2(binary_factors, levels, ~ subset(data, get(.x) == .y),
                         subset(data, get("Diabetes_binary") == 1))
  data[, "BMI"] <- purrr::map_dbl(data[, "BMI"],
                                  ~ if (.x < 15) return(15)
                                  else if (.x > 17) return(17)
                                  else return(.x))
  dataframes <- append(list(data), subsets)
  after <- purrr::map_dbl(dataframes, ~ predict_diabetic_proportion(model, .x))

  counterfactual <- data.frame(Before = before,
                               After = after,
                               Change = after - before)
  rownames(counterfactual) <- c("BMI", binary_factors)
  return(counterfactual)
}
