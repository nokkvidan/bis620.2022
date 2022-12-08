

test_that(
  "The boost_model() runs with test data",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, ]
    test <- diabetes[sample == 2, ]
    xg_fit <- boost_model("Diabetes_binary", train, test)
    expect_true(inherits(xg_fit, "list"))
    expect_true(inherits(xg_fit[[6]], "roc"))
  }
)

test_that(
  "The boost_model() runs with no test data",
  {
    data(diabetes)
    xg_fit <- boost_model("Diabetes_binary", diabetes)
    expect_true(inherits(xg_fit, "xgb.Booster"))
  }
)
