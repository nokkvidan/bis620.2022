
# Test inputs
test_that(
  "Test for vis_num(): check for data frame with only binary columns",
  {
    data(diabetes)
    df_dist <- diabetes[, c("Diabetes_binary", "Sex")]
    vis_num(df_dist)
  }
)

# Test for covr, devtools:test() skipped due to lack of output
test_that(
  "Test for vis_num(): overall test of function",
  {
    data(diabetes)
    vis_num(diabetes)
  }
)
