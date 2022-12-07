

test_that(
  "The vis_num() runs with no errors",
  {
    df <- read.csv("/Users/elisaloy/Desktop/diabetes_binary.csv")
    vis_num(df)
  }
)
