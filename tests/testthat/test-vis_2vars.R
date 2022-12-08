
################################################################################
############################## ggplot Inheritance ##############################

test_that(
  "The vis_2vars() returns a ggplot object for: factor v cont",
  {
    data(diabetes)
    p <- vis_2vars(diabetes, "Diabetes_binary", "BMI")
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The vis_2vars() returns a ggplot object for: cont v factor",
  {
    data(diabetes)
    p <- vis_2vars(diabetes, "BMI", "Diabetes_binary")
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The vis_2vars() returns a ggplot object for: factor v factor",
  {
    data(diabetes)
    p <- vis_2vars(diabetes, "Diabetes_binary", "Smoker")
    expect_true(inherits(p, "gg"))
  }
)


test_that(
  "The vis_2vars() returns a ggplot object for: cont v cont",
  {
    data(diabetes)
    p <- vis_2vars(diabetes, "BMI", "MentHlth")
    expect_true(inherits(p, "gg"))
  }
)
