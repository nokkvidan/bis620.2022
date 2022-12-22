# Test counterfactual for random forest
test_that(
  "Test for eval_rf_and_glm(): returns counterfactual table for random forest",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, ]
    test <- diabetes[sample == 2, ]
    rf_fit <- rf_model("Diabetes_binary", train, test)
    df <- eval_rf_and_glm(train, test, rf_fit, type = "prob")
    expect_true(inherits(df, "data.frame"))
  }
)

# Test counterfactual for glm
test_that(
  "Test for eval_rf_and_glm(): returns counterfactual table for glm",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, ]
    test <- diabetes[sample == 2, ]
    glm_fit <- glm_model("Diabetes_binary", train, test, optimize = "manual")
    df <- eval_rf_and_glm(train, test, glm_fit, type = "response")
    expect_true(inherits(df, "data.frame"))
  }
)
