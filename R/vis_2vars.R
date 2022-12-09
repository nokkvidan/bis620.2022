#' @title vis_2vars
#' @param d diabetes data frame
#' @param x an arbitrary variable in diabetes data frame
#' @param y an arbitrary variable in diabetes data frame
#' @importFrom ggplot2 ggplot geom_bar geom_violin aes
#' @examples \dontrun{
#' vis_2vars(df, 'Diabetes_binary', 'BMI')
#' }
#' @export
vis_2vars <- function(d, x, y) {
  if (x %!in% colnames(train)) {
    stop(paste0(x, " is not a variable in your data"))
  }
  if (y %!in% colnames(train)) {
    stop(paste0(y, " is not a variable in your data"))
  }
  fac1 <- length(unique(d[, x])) <= 15
  fac2 <- length(unique(d[, y])) <= 15
  if (fac1 && fac2) {
    d[, x] <- as.factor(d[, x])
    d[, y] <- as.factor(d[, y])
    p <- ggplot(d, aes(x = eval(str2lang(x)), fill = eval(str2lang(y)),
                       group = eval(str2lang(y)))) +
      ggplot2::geom_bar(position = "dodge") +
      labs(fill=y)
    subtitle <- "Barplot visualization"
  } else if (fac1 && !fac2) {
    d[, x] <- as.factor(d[, x])
    p <- ggplot(d) +
      ggplot2::geom_violin(aes(x = eval(str2lang(x)), y = eval(str2lang(y))))
    subtitle <- "Violinplot visualization"
  } else if (!fac1 && fac2) {
    d[, y] <- as.factor(d[, y])
    p <- ggplot(d) +
      ggplot2::geom_violin(aes(x = eval(str2lang(x)), y = eval(str2lang(y))))
    subtitle <- "Violinplot visualization"
  } else if (!fac1 && !fac2) {
    p <- ggplot(d) +
      ggplot2::geom_point(aes(x = eval(str2lang(x)), y = eval(str2lang(y))))
    subtitle <- "Scatterplot visualization"
  }
  title <- paste0("Variables: ", x, " & ", y)
  p <- p +
    ggplot2::labs(title = title, subtitle = subtitle, x = x, y = y) +
    ggplot2::theme_bw()
  return(p)
}
