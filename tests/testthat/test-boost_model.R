
# Test Inputs
test_that(
  "Test for boost_model(): y is a variable in the train data",
  {
    data(diabetes)
    expect_error(boost_model("Diabetes", diabetes))
  }
)

test_that(
  "Test for boost_model(): y is a variable in the test data, note that this
   should throw an error for test and train columns not matching",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, 1:4]
    test <- diabetes[sample == 2, 2:4]
    expect_error(boost_model("Diabetes_binary", train, test))
  }
)

test_that(
  "Test for boost_model(): y is a variable in the test data",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, 1:4]
    test <- diabetes[sample == 2, 1:5]
    expect_error(boost_model("Diabetes_binary", train, test))
  }
)

# Test Outputs
test_that(
  "Test for boost_model(): runs with test data",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, ]
    test <- diabetes[sample == 2, ]
    xg_fit <- boost_model("Diabetes_binary", train, test)
    expect_true(inherits(xg_fit, "list"))
    expect_true(inherits(xg_fit[[1]], "xgb.Booster"))
    expect_true(inherits(xg_fit[[2]], "confusionMatrix"))
    expect_true(inherits(xg_fit[[3]], "data.table"))
    expect_true(inherits(xg_fit[[4]], "PRROC"))
    expect_true(inherits(xg_fit[[5]], "PRROC"))
    expect_true(inherits(xg_fit[[6]], "roc"))
  }
)

test_that(
  "Test that boost_model(): runs with no test data",
  {
    data(diabetes)
    xg_fit <- boost_model("Diabetes_binary", diabetes)
    expect_true(inherits(xg_fit, "xgb.Booster"))
  }
)
