#' @description Formats categorical variable name for use in formula() and
#' as.formula
#' object
#' @title split_string
#' @param string character name of model variable
#' is an optional parameter with NULL default and triggers evaluation otherwise
#' @return if test is NULL, it returns a xgb.Booster object, if a test object is
#' added then a list of 6 objects. The first is a xgb.Booster object, the second
#' is a confusion matrix, the third is an importance matrix, the fourth is a
#' PRROC object for the ROC curve, the fifth is a PRROC object for a precision-
#' recall curve, the sixth is an roc object for the roc results
#' @importFrom stringr str_remove
#' @examples \dontrun{
#' split_string("CategoricalLevel10")
#' split_string("CategoricalLevel10TRUE")
#' }
#' @export
split_string <- function(string) {
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
