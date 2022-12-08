



test_that(
  "The glm_model() runs with no errors with test data and stepAIC optimization",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, ]
    test <- diabetes[sample == 2, ]
    glm_fit <- glm_model(train, "Diabetes_binary", test, optimize = "stepAIC")
    expect_true(inherits(glm_fit, "list"))
    expect_true(inherits(glm_fit[[7]], "roc"))
  }
)

test_that(
  "The glm_model() runs with no errors with test data and manual optimization",
  {
    data(diabetes)
    sample <- sample(c(1, 2), nrow(diabetes), replace = TRUE, prob = c(.8, .2))
    train <- diabetes[sample == 1, ]
    test <- diabetes[sample == 2, ]
    glm_fit <- glm_model(diabetes, "Diabetes_binary", test, optimize = "manual")
    expect_true(inherits(glm_fit, "list"))
    expect_true(inherits(glm_fit[[7]], "roc"))
  }
)


test_that(
  "The glm_model() runs with no errors with no data and no optimization",
  {
    data(diabetes)
    glm_fit <- glm_model(diabetes, "Diabetes_binary")
    expect_true(inherits(glm_fit, "glm"))
  }
)
