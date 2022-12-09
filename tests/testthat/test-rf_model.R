
# Test Inputs
test_that(
  "Test for rf_model(): y is a variable in the train data",
  {
    data(diabetes)
    expect_error(rf_model("Diabetes", diabetes))
  }
)

test_that(
  "Test for rf_model(): y is a variable in the test data, note that this
   should throw an error for test and train columns not matching",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, 1:4]
    test <- diabetes[sample == 2, 2:4]
    expect_error(rf_model("Diabetes_binary", train, test))
  }
)

test_that(
  "Test for rf_model(): check if test and train have the same column names",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, 1:4]
    test <- diabetes[sample == 2, 1:5]
    expect_error(rf_model("Diabetes_binary", train, test))
  }
)

test_that(
  "Test for rf_model(): check if we have a valid optimize",
  {
    data(diabetes)
    expect_error(rf_model("Diabetes_binary", diabetes, optimize = "manual"))
  }
)

# Test Outputs
test_that(
  "Test for glm_model(): run for no test data and with optimization",
  {
    data(diabetes)
    rf_fit <- rf_model("Diabetes_binary", diabetes, optimize = TRUE)
    expect_true(inherits(rf_fit, "randomForest"))
  }
)

test_that(
  "Test for rf_model(): runs with test data and no optimization",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, ]
    test <- diabetes[sample == 2, ]
    rf_fit <- rf_model("Diabetes_binary", train, test)
    expect_true(inherits(rf_fit, "list"))
    expect_true(inherits(rf_fit[[1]], "randomForest"))
    expect_true(inherits(rf_fit[[2]], "list"))
    expect_true(inherits(rf_fit[[3]], "list"))
    expect_true(inherits(rf_fit[[4]], "list"))
    expect_true(inherits(rf_fit[[5]], "data.frame"))
    expect_true(inherits(rf_fit[[6]], "confusionMatrix"))
    expect_true(inherits(rf_fit[[7]], "roc"))
  }
)
