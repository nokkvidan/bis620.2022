#' @description Visualize the distributions of the non categorical variables in
#' the data set
#' @title vis_dist
#' @param d a data frame
#' @return a historgram and qqPlot for all continuous variables in given data
#' @importFrom car qqPlot
#' @importFrom graphics hist par
#' @examples \dontrun{
#' data(diabetes)
#' vis_dist(diabetes)
#' }
#' @export
vis_dist <- function(d) {
  ## Filter the data
  # Check which variables have more than 2 unique values to remove factors
  bin <- apply(d, 2, function(x) length(unique(x)) == 2)
  # Remove the identified factors from the data frame
  d[, bin] <- sapply(d[, bin], as.logical)
  # Use sapply to grab only the columns that have more than 2 unique values for
  # their variable but that are also numeric (aka words remove strings columns)
  df_num <- d[, sapply(d, is.numeric)]
  ## Check parameters
  # Check that the resulting filtered data is not empty
  if (ncol(df_num) == 0) {
    stop("Please input data with numeric columns")
  }
  ## Begin the function
  # Split the window to create side by side plots
  par(mfrow = c(2, 2))
  # Plotting for loop for each continuous variable
  for (i in seq_len(ncol(df_num))) {
    # Create histogram of the current variable
    hist(df_num[, i], col = "red", pch = 19, xlab = names(df_num)[i],
         main = paste("Histogram of ", names(df_num)[i], sep = ""))
    # Plot qqPlot for current variable
    car::qqPlot(df_num[, i], col = "red", pch = 19, ylab = names(df_num)[i],
           main = paste("QQ-Plot for ", names(df_num)[i], sep = ""))
  }
  # Return the window to regular dimensions
  par(mfrow = c(1, 1))
}
