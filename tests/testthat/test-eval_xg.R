# Test counterfactual table for XGBoost
test_that(
  "Test for eval_xg(): returns counterfactual data frame",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, ]
    test <- diabetes[sample == 2, ]
    xg_fit <- boost_model("Diabetes_binary", train, test)
    df <- eval_xg(train, test, xg_fit, y_name = "Diabetes_binary")
    expect_true(inherits(df, "data.frame"))
  }
)
