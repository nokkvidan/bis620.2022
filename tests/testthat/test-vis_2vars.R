
################################################################################
############################## ggplot Inheritance ##############################

test_that(
  "The vis_2vars() returns a ggplot object for: factor v cont",
  {
    df <- read.csv("/Users/elisaloy/Desktop/diabetes_binary.csv")
    p <- vis_2vars(df, 'Diabetes_binary', 'BMI')
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The vis_2vars() returns a ggplot object for: cont v factor",
  {
    df <- read.csv("/Users/elisaloy/Desktop/diabetes_binary.csv")
    p <- vis_2vars(df, 'BMI', 'Diabetes_binary')
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The vis_2vars() returns a ggplot object for: factor v factor",
  {
    df <- read.csv("/Users/elisaloy/Desktop/diabetes_binary.csv")
    p <- vis_2vars(df, 'Diabetes_binary', 'Smoker')
    expect_true(inherits(p, "gg"))
  }
)


test_that(
  "The vis_2vars() returns a ggplot object for: cont v cont",
  {
    df <- read.csv("/Users/elisaloy/Desktop/diabetes_binary.csv")
    p <- vis_2vars(df, 'BMI', 'MentHlth')
    expect_true(inherits(p, "gg"))
  }
)

