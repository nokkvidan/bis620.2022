#' @param d diabetes data frame
#' @param y an arbitrary variable in diabetes data frame
#' @param x an arbitrary variable in diabetes data frame
#' @importFrom ggplot2 aes_string ggplot geom_line facet_grid aes

#' @examples
#' vis_2vars(df, 'Diabetes_binary', 'BMI')
#' @export
vis_2vars <- function(d, y, x) {
  col1 <- d[, x]
  col2 <- d[, y]
  fac1 <- length(unique(col1)) <= 15
  fac2 <- length(unique(col2)) <= 15
  if (fac1 && fac2) {
    p <- ggplot(d, aes(x = eval(str2lang(x)), fill = eval(str2lang(y)))) +
      geom_bar(position = "dodge")
    subtitle <- "Barplot visulization"
  } else if (fac1 && !fac2) {
    p <- ggplot(d) +
      geom_violin(aes(x = eval(str2lang(x)), y = eval(str2lang(y))))
    subtitle <- "Violinplot visulization"
  } else if (!fac1 && fac2) {
    p <- ggplot(d) +
      geom_violin(aes(x = eval(str2lang(x)), y = eval(str2lang(x))))
    subtitle <- "Violinplot visulization"
  } else if (!fac1 && !fac2) {
    p <- ggplot(d) +
      geom_point(aes(x = eval(str2lang(x)), y = eval(str2lang(y))))
    subtitle <- "Scatterplot visulization"
  }
  title <- paste0("Visulizing the variables: ", x, " & ", y)
  p <- p + labs(title = title, subtitle = subtitle, x = x, y = y) + theme_bw()
  return(p)
}

