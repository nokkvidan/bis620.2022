test_that(
  "Test for get_counterfactual(): returns a dataframe for random forest,
  xg boost, and glm models",
  {
    data(diabetes)

    rf_fit <- rf_model("Diabetes_binary", diabetes)
    counterfactual <- get_counterfactual(rf_fit, diabetes)
    expect_true(inherits(counterfactual, "data.frame"))

    xg_fit <- boost_model("Diabetes_binary", diabetes)
    counterfactual <- get_counterfactual(xg_fit, diabetes)
    expect_true(inherits(counterfactual, "data.frame"))

    glm_fit <- glm_model("Diabetes_binary", diabetes)
    counterfactual <- get_counterfactual(glm_fit, diabetes)
    expect_true(inherits(counterfactual, "data.frame"))
  }
)

test_that(
  "Test for get_counterfactual(): requires randomForest, xgb.Booster, or glm
  model",
  {
    data(diabetes)
    lm_model <- lm(Diabetes_binary ~ BMI, data = diabetes)
    expect_error(get_counterfactual(lm_model, diabetes))
  }
)
