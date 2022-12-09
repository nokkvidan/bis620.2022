#' @title vis_dist
#' @param d diabetes data frame
#' @importFrom car qqPlot
#' @importFrom graphics hist par
#' @examples \dontrun{
#' vis_dist(df)
#' }
#' @export
vis_dist <- function(d) {
  bin <- apply(d, 2, function(x) length(unique(x)) == 2)
  d[, bin] <- sapply(d[, bin], as.logical)
  df_num <- d[, sapply(d, is.numeric)]
  # Check parameters
  if (ncol(d_num) == 0) {
    stop("Please input data with numeric columns")
  }
  # Begin the function
  par(mfrow = c(2, 2))
  # Plotting for loop
  for (i in seq_len(ncol(df_num))) {
    hist(df_num[, i], col = "red", pch = 19, xlab = names(df_num)[i],
         main = paste("Histogram of ", names(df_num)[i], sep = ""))
    car::qqPlot(df_num[, i], col = "red", pch = 19, ylab = names(df_num)[i],
           main = paste("QQ-Plot for ", names(df_num)[i], sep = ""))
  }
}
