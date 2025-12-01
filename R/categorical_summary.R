#' Summarize Categorical Variables
#'
#' Provides counts and percentages for all factor or character variables.
#'
#' @param data A data frame.
#'
#' @return A tibble with variable, category, count, and percent.
#' @examples
#' \dontrun{
#' categorical_summary(iris)
#' }
#' @import dplyr
#' @import tibble
#' @import tidyr
#' @export
categorical_summary <- function(data) {
  cat_vars <- dplyr::select_if(data, function(x) is.factor(x) || is.character(x))
  if (ncol(cat_vars) == 0) stop("No categorical variables found.")

  result <- lapply(names(cat_vars), function(var) {
    dplyr::count(cat_vars, !!rlang::sym(var)) %>%
      dplyr::mutate(variable = var,
                    percent = round(n / sum(n) * 100, 2)) %>%
      dplyr::rename(category = !!var)
  })
  dplyr::bind_rows(result)
}
