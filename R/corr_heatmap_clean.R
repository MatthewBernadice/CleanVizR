#' Clean Correlation Heatmap
#'
#' @param data A data frame with numeric columns.
#' @param palette A Brewer palette.
#'
#' @return A ggplot2 heatmap plot.
#' @import ggplot2
#' @import dplyr
#' @import tidyr
#' @import RColorBrewer
#' @export
corr_heatmap_clean <- function(data, palette = "RdBu") {
  num_data <- dplyr::select_if(data, is.numeric)

  if (ncol(num_data) < 2) stop("Need at least two numeric variables.")

  corr_mat <- round(cor(num_data, use = "pairwise.complete.obs"), 2)

  corr_df <- tidyr::pivot_longer(
    as.data.frame(as.table(corr_mat)),
    cols = c("Var1", "Var2", "Freq"),
    names_to = c(".value"),
    values_to = "correlation"
  )

  ggplot(corr_df, aes(Var1, Var2, fill = Freq)) +
    geom_tile() +
    geom_text(aes(label = Freq), size = 4) +
    scale_fill_distiller(palette = palette, limits = c(-1, 1)) +
    theme_minimal(base_size = 14) +
    labs(title = "Correlation Heatmap", x = "", y = "")
}
