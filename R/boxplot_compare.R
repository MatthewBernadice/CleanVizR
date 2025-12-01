#' Compare Numeric Variable Across Categories with a Boxplot
#'
#' Creates a boxplot of a numeric variable grouped by a categorical variable.
#'
#' @param data A data frame.
#' @param numeric_var Name of numeric variable (string).
#' @param group_var Name of categorical grouping variable (string).
#' @param palette Optional Brewer palette name (default "Set2").
#'
#' @return A ggplot2 boxplot.
#' @examples
#' \dontrun{
#' boxplot_compare(iris, "Sepal.Length", "Species")
#' }
#' @import ggplot2
#' @import RColorBrewer
#' @import dplyr
#' @export
boxplot_compare <- function(data, numeric_var, group_var, palette = "Set2") {
  if (!numeric_var %in% names(data)) stop("numeric_var not found in data.")
  if (!group_var %in% names(data)) stop("group_var not found in data.")
  if (!is.numeric(data[[numeric_var]])) stop("numeric_var must be numeric.")

  ggplot2::ggplot(data, ggplot2::aes_string(x = group_var, y = numeric_var, fill = group_var)) +
    ggplot2::geom_boxplot() +
    ggplot2::scale_fill_brewer(palette = palette) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(title = paste("Boxplot of", numeric_var, "by", group_var),
                  x = group_var, y = numeric_var)
}
