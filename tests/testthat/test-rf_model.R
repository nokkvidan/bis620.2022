





test_that(
  "The rf_model() runs with no errors with no optimization and TRUE",
  {
    df <- read.csv("/Users/elisaloy/Desktop/diabetes_binary.csv")
    rf_fit <- rf_model("Diabetes_binary", df)
    expect_true(inherits(rf_fit, "randomForest"))
  }
)


test_that(
  "The rf_model() runs with no errors with TRUE optimization and test data",
  {
    df <- read.csv("/Users/elisaloy/Desktop/diabetes_binary.csv")
    sample <- sample(c(1, 2), nrow(df), replace = TRUE, prob = c(0.8, 0.2))
    train <- df[sample == 1, ]
    test <- df[sample == 2, ]
    rf_fit <- rf_model("Diabetes_binary", train, test, optimize = TRUE)
    expect_true(inherits(rf_fit, "list"))
    expect_true(inherits(rf_fit[[7]], "roc"))
  }
)
