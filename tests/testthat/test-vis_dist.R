
# Test inputs
test_that(
  "Test for vis_dist(): check for data frame with only binary columns",
  {
    data(diabetes)
    df_dist <- diabetes[, c("Diabetes_binary", "Sex")]
    vis_dist(df_dist)
  }
)

# Test for covr, devtools:test() skipped due to lack of output
test_that(
  "Test for vis_dist(): overall test of function",
    data(diabetes)
    vis_dist(diabetes)
  }
)
