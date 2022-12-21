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
  
  firstNumber <- sub(".*?(\\d).*", "\\1", string)
  if (nchar(firstNumber) > 1) {
    # no number in string
    return(string)
  }
  
  indexOfFirstNumber <- unlist(gregexpr(firstNumber, string))
  prefix <- substring(string, 1, indexOfFirstNumber - 1)
  suffix <- substring(string, indexOfFirstNumber)
  return(paste0("I(", prefix, "==", suffix, ")"))
}
