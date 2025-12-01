#' Clean and Standardize Column Names
#'
#' @param data A data frame or tibble.
#'
#' @return Data with cleaned column names.
#' @import tibble
#' @export
clean_names_simple <- function(data) {
  if (!is.data.frame(data)) stop("`data` must be a data frame.")

  names(data) <- tolower(names(data))
  names(data) <- gsub("[^a-z0-9]+", "_", names(data))
  names(data) <- gsub("^_|_$", "", names(data))
  names(data) <- make.unique(names(data))

  tibble::as_tibble(data)
}
