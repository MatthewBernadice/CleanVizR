#' Summarize Missing Values in a Dataset
#'
#' Quickly generates a summary of missing values for each column in a data frame.
#'
#' @param data A data frame or tibble.
#'
#' @return A tibble with columns: variable, missing_count, missing_percent.
#' @examples
#' \dontrun{
#' summarize_missing(airquality)
#' }
#' @import dplyr
#' @import tibble
#' @export
summarize_missing <- function(data) {
  if (!is.data.frame(data)) stop("Input must be a data frame.")

  tibble::tibble(
    variable = names(data),
    missing_count = sapply(data, function(x) sum(is.na(x))),
    missing_percent = round(sapply(data, function(x) mean(is.na(x)) * 100), 2)
  )
}
