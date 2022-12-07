#' @param d diabetes data frame
#' @importFrom ggplot2 aes_string ggplot geom_boxplot facet_grid aes theme_bw
#' @importFrom gridExtra grid.arrange
#' @examples
#' vis_num(df)
#' @export
vis_num <- function(d) {
  d_num <- d[, sapply(d, is.numeric)]
  d_num_scaled <- data.frame(
    apply(d_num, 2, function(x) (x - min(x)) / (max(x) - min(x))))
  p1 <- ggplot(stack(d_num), aes(x = ind, y = values)) +
    geom_boxplot() +
    labs(title = "Visulizing Numeric Variables",
         subtitle = "Boxplot visulization",
         x = "Values",
         y = "Variables") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  p2 <- ggplot(stack(d_num_scaled), aes(x = ind, y = values)) +
    geom_boxplot() +
    labs(title = "Visulizing Scaled Numeric Variables",
         subtitle = "Boxplot visulization",
         x = "Scaled Values",
         y = "Variables") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))
  return(gridExtra::grid.arrange(p1, p2, ncol = 2))
}
