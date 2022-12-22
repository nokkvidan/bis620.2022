
# Test Inputs
test_that(
  "Test for glm_model(): y is a variable in the train data",
  {
    data(diabetes)
    expect_error(glm_model("Diabetes", diabetes))
  }
)

test_that(
  "Test for glm_model(): y is a variable in the test data, note that this
  should throw an error for test and train columns not matching",
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
    expect_error(glm_model("Diabetes_binary", diabetes, optimize = TRUE))
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
    expect_true(inherits(glm_fit[[1]], "glm"))
    expect_true(inherits(glm_fit[[2]], "list"))
    expect_true(inherits(glm_fit[[3]], "list"))
    expect_true(inherits(glm_fit[[4]], "list"))
    expect_true(inherits(glm_fit[[5]], "data.frame"))
    expect_true(inherits(glm_fit[[6]], "confusionMatrix"))
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

# Tests for extract_categorical_name
test_that(
  "Test for extract_categorical_name(): TRUE/FALSE",
  {
    expect_true(inherits(extract_categorical_name("stringTRUE"), "character"))
  }
)

test_that(
  "Test for extract_categorical_name(): for names with numbers",
  {
    expect_true(inherits(extract_categorical_name("string5"), "character"))
  }
)
