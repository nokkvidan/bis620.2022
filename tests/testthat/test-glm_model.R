



test_that(
  "The glm_model() runs with no errors with test data and stepAIC optimization",
  {
    df <- read.csv("/Users/elisaloy/Desktop/diabetes_binary.csv")
    sample <- sample(c(1, 2), nrow(df), replace=TRUE, prob=c(0.8, 0.2))
    train <- df[sample == 1, ]
    test <- df[sample == 2, ]
    glm.model <- glm_model(train, "Diabetes_binary", test, optimize = "stepAIC")
    expect_true(inherits(glm.model, "list"))
    expect_true(inherits(glm.model[[7]], "roc"))
  }
)

test_that(
  "The glm_model() runs with no errors with test data and manual optimization",
  {
    df <- read.csv("/Users/elisaloy/Desktop/diabetes_binary.csv")
    sample <- sample(c(1, 2), nrow(df), replace=TRUE, prob=c(0.8, 0.2))
    train <- df[sample == 1, ]
    test <- df[sample == 2, ]
    glm.model <- glm_model(df, "Diabetes_binary", test, optimize = "manual")
    expect_true(inherits(glm.model, "list"))
    expect_true(inherits(glm.model[[7]], "roc"))
  }
)


test_that(
  "The glm_model() runs with no errors with no data and no optimization",
  {
    df <- read.csv("/Users/elisaloy/Desktop/diabetes_binary.csv")
    glm.model <- glm_model(df, "Diabetes_binary")
    expect_true(inherits(glm.model, "glm"))
  }
)
