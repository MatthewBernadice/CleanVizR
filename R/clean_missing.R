#' Clean Missing Values in a Dataset
#'
#' Quickly remove or impute missing values in a data frame.
#'
#' @param data A data frame or tibble.
#' @param method Character. One of "remove", "mean", "median", or "mode".
#'               "remove" drops rows with any NAs; others impute numeric columns.
#' @return A tibble with missing values handled.
#' @examples
#' \dontrun{
#' df <- data.frame(a = c(1, NA, 3), b = c("x", "y", NA))
#' clean_missing(df, method = "mean")
#' }
#' @imports tibble
#' @imports stats
#' @export
clean_missing <- function(data, method = c("remove", "mean", "median", "mode")) {
  method <- match.arg(method)
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.", call. = FALSE)

  impute_value <- function(x) {
    if (!is.numeric(x)) return(x)
    if (method == "mean")   return(ifelse(is.na(x), mean(x, na.rm = TRUE), x))
    if (method == "median") return(ifelse(is.na(x), median(x, na.rm = TRUE), x))
    if (method == "mode") {
      m <- names(sort(table(x), decreasing = TRUE))[1]
      return(ifelse(is.na(x), as.numeric(m), x))
    }
    x
  }

  if (method == "remove") {
    return(tibble::as_tibble(stats::na.omit(data)))
  }

  data[] <- lapply(data, impute_value)
  tibble::as_tibble(data)
}
