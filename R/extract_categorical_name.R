#' @description Formats categorical variable names for use in formula() and
#' as.formula, specifically when R/glm_model() extracts significant variables.
#' object
#' @title extract_categorical_name
#' @param string character name of model variable
#' @return formatted categorical variable name
#' @importFrom stringr str_remove
#' @examples \dontrun{
#' extract_categorical_name("Education10")
#' extract_categorical_name("Education10TRUE")
#' }
#' @export
extract_categorical_name <- function(string) {
  if (endsWith(string, "TRUE") || endsWith(string, "FALSE")) {
    return(stringr::str_remove(string, "TRUE|FALSE"))
  }

  first_number <- sub(".*?(\\d).*", "\\1", string)
  if (suppressWarnings(is.na(as.numeric(first_number)))) {
    # no number in string
    return(string)
  }

  index_of_first_number <- unlist(gregexpr(first_number, string))
  prefix <- substring(string, 1, index_of_first_number - 1)
  suffix <- substring(string, index_of_first_number)
  return(paste0("I(", prefix, "==", suffix, ")"))
}
