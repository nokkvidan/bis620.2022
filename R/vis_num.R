#' @description Visualize the non categorical variables in the dataset with
#' boxplots. Shows both scaled and non scaled variables
#' @title vis_num
#' @param d a data frame
#' @return a grid.arrange of two ggplots: one of side by side boxplots of the
#' raw data for the numeric variables and one of side by side boxplots of the
#' scaled data for the numeric variables such that they all lie between 0 and 1
#' @importFrom ggplot2 aes_string ggplot geom_boxplot geom_point
#' facet_grid aes theme_bw labs theme element_text
#' @importFrom gridExtra grid.arrange
#' @importFrom utils stack
#' @examples \dontrun{
#' data(diabetes)
#' vis_num(diabetes)
#' }
#' @export
vis_num <- function(d) {
  ## Filter the data
  # Check which variables have more than 2 unique values to remove factors
  bin <- apply(d, 2, function(x) length(unique(x)) == 2)
  # Remove the identified factors from the data frame
  d[, bin] <- sapply(d[, bin], as.logical)
  # Use sapply to grab only the columns that have more than 2 unique values for
  # their variable but that are also numeric (aka words remove strings columns)
  d_num <- d[, sapply(d, is.numeric)]
  ## Check parameters
  # Check that the resulting filtered data is not empty
  if (ncol(d_num) == 0) {
    stop("Please input data with numeric columns")
  }
  ## Begin the function
  # Scale all the variables to fit it from 0 to 1
  d_num_scaled <- data.frame(
    apply(d_num, 2, function(x) (x - min(x)) / (max(x) - min(x))))
  # Creating the plots
  # Create raw data boxplots of numeric variables
  p1 <- ggplot(stack(d_num), aes(x = ind, y = values)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = "Visulizing Numeric Variables",
                  subtitle = "Boxplot visulization",
                  x = "Values",
                  y = "Variables") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5, hjust = 1))
  # Create scaled data boxplots of continuous numeric variables
  p2 <- ggplot(stack(d_num_scaled), aes(x = ind, y = values)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = "Visulizing Scaled Numeric Variables",
                  subtitle = "Boxplot visulization",
                  x = "Scaled Values",
                  y = "Variables") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5, hjust = 1))
  # Return the two ggplot objects in a grid.arrange
  return(gridExtra::grid.arrange(p1, p2, ncol = 2))
}
