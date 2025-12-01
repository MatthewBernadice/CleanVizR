#' Simple Text Cleaning
#'
#' @param text A character vector.
#'
#' @return A cleaned character vector.
#' @import qdap
#' @import dplyr
#' @export
text_cleaner <- function(text) {
  if (!is.character(text)) stop("Input must be a character vector.")

  text %>%
    qdap::bracketX() %>%         # remove bracketed text
    qdap::rm_stopwords() %>%     # remove stopwords
    tolower() %>%
    gsub("[[:punct:]]", "", .) %>%
    gsub("\\s+", " ", .) %>%
    trimws()
}
