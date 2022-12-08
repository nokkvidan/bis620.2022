#' @title vis_num
#' @param d diabetes data frame
#' @importFrom ggplot2 aes_string ggplot geom_boxplot geom_point
#' facet_grid aes theme_bw labs theme element_text
#' @importFrom gridExtra grid.arrange
#' @importFrom utils stack
#' @examples \dontrun{
#' vis_num(df)
#' }
#' @export
vis_num <- function(d) {
  d_num <- d[, sapply(d, is.numeric)]
  d_num_scaled <- data.frame(
    apply(d_num, 2, function(x) (x - min(x)) / (max(x) - min(x))))
  p1 <- ggplot(stack(d_num), aes(x = ind, y = values)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = "Visulizing Numeric Variables",
                  subtitle = "Boxplot visulization",
                  x = "Values",
                  y = "Variables") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5, hjust = 1))
  p2 <- ggplot(stack(d_num_scaled), aes(x = ind, y = values)) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(title = "Visulizing Scaled Numeric Variables",
                  subtitle = "Boxplot visulization",
                  x = "Scaled Values",
                  y = "Variables") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90,
                                                       vjust = 0.5, hjust = 1))
  return(gridExtra::grid.arrange(p1, p2, ncol = 2))
}
