test_that(
  "The accel_plot() returns a ggplot object. spectral",
  {
    data(ukb_accel)
    p <- ukb_accel[1:100, ] |>
      spectral_signature(take_log = TRUE) |>
      accel_plot()
    expect_true(inherits(p, "gg"))
  }
)
