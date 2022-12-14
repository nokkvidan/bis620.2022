library(stringr)

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