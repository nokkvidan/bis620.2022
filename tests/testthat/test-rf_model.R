





test_that(
  "The rf_model() runs with no errors with no optimization and TRUE",
  {
    data(diabetes)
    rf_fit <- rf_model("Diabetes_binary", diabetes)
    expect_true(inherits(rf_fit, "randomForest"))
  }
)


test_that(
  "The rf_model() runs with no errors with TRUE optimization and test data",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, ]
    test <- diabetes[sample == 2, ]
    rf_fit <- rf_model("Diabetes_binary", train, test, optimize = TRUE)
    expect_true(inherits(rf_fit, "list"))
    expect_true(inherits(rf_fit[[7]], "roc"))
  }
)
