#' Quick Histogram Visualization
#'
#' @param data A data frame.
#' @param var A numeric variable in the dataset.
#' @param palette A Brewer palette name.
#'
#' @return A ggplot2 histogram.
#' @import ggplot2
#' @import RColorBrewer
#' @export
quick_hist <- function(data, var, palette = "Blues") {
  if (!var %in% names(data)) stop("Variable not found.")
  if (!is.numeric(data[[var]])) stop("`var` must be numeric.")

  ggplot(data, aes_string(x = var)) +
    geom_histogram(fill = RColorBrewer::brewer.pal(9, palette)[6],
                   color = "white", bins = 30) +
    ggplot2::theme_minimal(base_size = 14) +
    labs(title = paste("Distribution of", var),
         x = var,
         y = "Count")
}
