#' returnNumSlots - a helper function to return character summary of a numeric based on the groupChar
#'  
#' This function returns the text from numericSummary; the returned value is formatted such that it can be directly placed in the table 1 column
#' @param eSet - the eSet or data.frame sent to table1
#' @param groupChar - a factor column present in eSet by which to group samples and summarize char
#' @param group - which factor level from groupChar is summarized
#' @param char - which characteristic is summarized from eSet
#' @export
#' @examples
#' returnNumSlots(eSet, "CancerStatus", "CancerPositive", "Age")

returnNumSlots <- function(eSet, groupChar, group, char){
  return(numericSummary(eSet[[char]][eSet[[groupChar]]==group]))
}