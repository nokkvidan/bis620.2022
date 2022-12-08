

test_that(
  "The boost_model() runs with test data",
  {
    df <- read.csv("/Users/elisaloy/Desktop/diabetes_binary.csv")
    sample <- sample(c(1, 2), nrow(df), replace = TRUE, prob = c(0.8, 0.2))
    train <- df[sample == 1, ]
    test <- df[sample == 2, ]
    xg_fit <- boost_model("Diabetes_binary", train, test)
    expect_true(inherits(xg_fit, "list"))
    expect_true(inherits(xg_fit[[6]], "roc"))
  }
)

test_that(
  "The boost_model() runs with no test data",
  {
    df <- read.csv("/Users/elisaloy/Desktop/diabetes_binary.csv")
    xg_fit <- boost_model("Diabetes_binary", df)
    expect_true(inherits(xg_fit, "xgb.Booster"))
  }
)
