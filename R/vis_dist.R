#' @param d diabetes data frame
#' @importFrom car qqPlot
#' @examples 
#' vis_dist(df)
#' @export
vis_dist <- function(d) {
  df_num <- df[, sapply(df, is.numeric)]
  par(mfrow = c(2, 2))
  # Plotting for loop
  for (i in seq_len(ncol(df_num))) {
    hist(df_num[, i], col = "red", pch = 19, xlab = names(df_num)[i],
         main = paste("Histogram of ", names(df_num)[i], sep = ""))
    qqPlot(df_num[, i], col = "red", pch = 19, ylab = names(df_num)[i],
           main = paste("QQ-Plot for ", names(df_num)[i], sep = ""))
  }
}