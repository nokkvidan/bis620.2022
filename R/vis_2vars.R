#' @description Visualize any two variables in the dataset
#' @title vis_2vars
#' @param d a data frame
#' @param x an arbitrary character variable in diabetes data frame
#' @param y an arbitrary character variable in diabetes data frame
#' @return a ggplot depending on the types of variables. Returns a violin
#' plot for one continuous and one factor variable, returns a bar plot for two
#' factor variables, and returns a scatterplot for two continuous variables
#' @importFrom ggplot2 ggplot geom_bar geom_violin aes
#' @examples \dontrun{
#' data(diabetes)
#' vis_2vars(diabetes, "BMI", "MentHlth")
#' }
#' @export
vis_2vars <- function(d, x, y) {
  ## Check parameters
  # Check that the given x variable exists in the data
  if (x %nin% colnames(d)) {
    stop(paste0(x, " is not a variable in your data"))
  }
  ## Check parameters
  # Check that the given y variable exists in the data
  if (y %nin% colnames(d)) {
    stop(paste0(y, " is not a variable in your data"))
  }
  ## Begin the function
  # Check the number of unique values in the two variables
  # If less than 15 then it's considered a factor, otherwise it's continuous
  fac1 <- length(unique(d[, x])) <= 15
  fac2 <- length(unique(d[, y])) <= 15
  # Different functions for each combination of variable types
  if (fac1 && fac2) {
    # If both have less than 15, then convert them to factors
    d[, x] <- as.factor(d[, x])
    d[, y] <- as.factor(d[, y])
    # The plot is a barplot or x split and colored by y
    p <- ggplot(d, aes(x = eval(str2lang(x)), fill = eval(str2lang(y)),
                       group = eval(str2lang(y)))) +
      ggplot2::geom_bar(position = "dodge") +
      labs(fill = y)
    subtitle <- "Barplot visualization"
  } else if (fac1 && !fac2) {
    # If x has less than 15 unique values and y does not, then:
    # Convert x to a factor
    d[, x] <- as.factor(d[, x])
    # Create a Violin plot of y split by x
    p <- ggplot(d) +
      ggplot2::geom_violin(aes(x = eval(str2lang(x)), y = eval(str2lang(y))))
    subtitle <- "Violinplot visualization"
  } else if (!fac1 && fac2) {
    # If y has less than 15 unique values and x does not, then:
    # Convert y to a factor
    d[, y] <- as.factor(d[, y])
    # Create a Violin plot of x split by y
    p <- ggplot(d) +
      ggplot2::geom_violin(aes(x = eval(str2lang(x)), y = eval(str2lang(y))))
    subtitle <- "Violinplot visualization"
  } else if (!fac1 && !fac2) {
    # If both x and y have more than 15 unique values, consider both continuous
    # Create a scatter plot with x and y on the appropriate axes
    p <- ggplot(d) +
      ggplot2::geom_point(aes(x = eval(str2lang(x)), y = eval(str2lang(y))))
    subtitle <- "Scatterplot visualization"
  }
  # Add a title for the x and y variables
  title <- paste0("Variables: ", x, " & ", y)
  # Add aesthetics to the plot and return
  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, x = x, y = y) +
    ggplot2::theme_bw()
  return(p)
}
