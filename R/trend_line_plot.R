#' Plot Trend Line of a Numeric Variable
#'
#' Creates a scatter plot with an optional trend line (linear model).
#'
#' @param data A data frame.
#' @param numeric_var Name of numeric variable (string).
#' @param index_var Optional sequential/index variable (string). Defaults to row number.
#'
#' @return A ggplot2 scatter plot with trend line.
#' @examples
#' \dontrun{
#' trend_line_plot(airquality, "Ozone")
#' }
#' @import ggplot2
#' @import dplyr
#' @export
trend_line_plot <- function(data, numeric_var, index_var = NULL) {
  if (!numeric_var %in% names(data)) stop("numeric_var not found.")
  if (!is.numeric(data[[numeric_var]])) stop("numeric_var must be numeric.")

  plot_data <- data
  if (is.null(index_var)) {
    plot_data$..index.. <- seq_len(nrow(plot_data))
    index_var <- "..index.."
  } else if (!index_var %in% names(plot_data)) stop("index_var not found.")

  ggplot2::ggplot(plot_data, ggplot2::aes_string(x = index_var, y = numeric_var)) +
    ggplot2::geom_point(color = "steelblue") +
    ggplot2::geom_smooth(method = "lm", color = "red", se = TRUE) +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::labs(title = paste("Trend of", numeric_var, "over", index_var),
                  x = index_var, y = numeric_var)
}
