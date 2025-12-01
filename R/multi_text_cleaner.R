#' Clean Multiple Text Columns in a Data Frame
#'
#' Applies basic text cleaning to multiple text columns using qdap.
#'
#' @param data A data frame.
#' @param cols Character vector of column names to clean.
#'
#' @return Data frame with cleaned text columns.
#' @examples
#' \dontrun{
#' df <- data.frame(comment = c("Hello [test]!", "Remove stopwords please"))
#' multi_text_cleaner(df, c("comment"))
#' }
#' @import qdap
#' @import dplyr
#' @import tibble
#' @export
multi_text_cleaner <- function(data, cols) {
  if (!all(cols %in% names(data))) stop("Some cols not found in data.")

  data %>%
    dplyr::mutate(dplyr::across(all_of(cols), ~ .x %>%
                                  qdap::bracketX() %>%
                                  qdap::rm_stopwords() %>%
                                  tolower() %>%
                                  gsub("[[:punct:]]", "", .) %>%
                                  gsub("\\s+", " ", .) %>%
                                  trimws()
    ))
}
