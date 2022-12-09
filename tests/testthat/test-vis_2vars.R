
# Test Inputs
test_that(
  "Test for vis_2vars(): x is a variable in the train data",
  {
    data(diabetes)
    expect_error(vis_2vars(diabetes, "Diabetes", "BMI"))
  }
)

test_that(
  "Test for vis_2vars(): y is a variable in the train data",
  {
    data(diabetes)
    expect_error(vis_2vars(diabetes, "Diabetes_binary", "BMX"))
  }
)

# Test Outputs
test_that(
  "Test for vis_2vars(): check a ggplot return for factor v cont",
  {
    data(diabetes)
    p <- vis_2vars(diabetes, "Diabetes_binary", "BMI")
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "Test for vis_2vars(): check a ggplot return for cont v factor",
  {
    data(diabetes)
    p <- vis_2vars(diabetes, "BMI", "Diabetes_binary")
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "Test for vis_2vars(): check a ggplot return for factor v factor",
  {
    data(diabetes)
    p <- vis_2vars(diabetes, "Diabetes_binary", "Smoker")
    expect_true(inherits(p, "gg"))
  }
)


test_that(
  "Test for vis_2vars(): check a ggplot return for cont v cont",
  {
    data(diabetes)
    p <- vis_2vars(diabetes, "BMI", "MentHlth")
    expect_true(inherits(p, "gg"))
  }
)
