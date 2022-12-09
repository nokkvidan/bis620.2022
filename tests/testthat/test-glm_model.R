
# Test Inputs
test_that(
  "Test for glm_model(): y is a variable in the train data",
  {
    data(diabetes)
    expect_error(glm_model("Diabetes", diabetes))
  }
)

test_that(
  "Test for glm_model(): y is a variable in the test data",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, 1:4]
    test <- diabetes[sample == 2, 2:4]
    expect_error(glm_model("Diabetes_binary", train, test))
  }
)

test_that(
  "Test for glm_model(): check if test and train have the same column names",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, 1:4]
    test <- diabetes[sample == 2, 1:5]
    expect_error(glm_model("Diabetes_binary", train, test))
  }
)

test_that(
  "Test for glm_model(): check if we have a valid optimize",
  {
    data(diabetes)
    expect_error(glm_model("Diabetes", diabetes, optimize = TRUE))
  }
)

# Test Outputs
test_that(
  "Test for glm_model(): run for no test data and no optimization",
  {
    data(diabetes)
    glm_fit <- glm_model("Diabetes_binary", diabetes)
    expect_true(inherits(glm_fit, "glm"))
  }
)

test_that(
  "Test for glm_model(): runs with test data",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, ]
    test <- diabetes[sample == 2, ]
    glm_fit <- glm_model("Diabetes_binary", train, test)
    expect_true(inherits(glm_fit, "list"))
    expect_true(inherits(glm_fit[[7]], "roc"))
  }
)

test_that(
  "Test for glm_model(): run for no test data and stepAIC optimization",
  {
    data(diabetes)
    glm_fit <- glm_model("Diabetes_binary", diabetes, optimize = "stepAIC")
    expect_true(inherits(glm_fit, "glm"))
  }
)

test_that(
  "Test for glm_model(): run for no test data and manual optimization",
  {
    data(diabetes)
    glm_fit <- glm_model("Diabetes_binary", diabetes, optimize = "manual")
    expect_true(inherits(glm_fit, "glm"))
  }
)

