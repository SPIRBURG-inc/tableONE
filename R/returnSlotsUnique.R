#' returnSlotsUnique - a helper function to help generate slots for table based on single patients/samples instead of groups (i.e. that in Campbell 2012)
#'  
#' This function returns a character representing the results of the summarization; meant for use when generating tables for patients instead of groups
#' @param eSet - the eSet or data.frame sent to table1
#' @param groupChar - a factor column present in eSet by which to group samples and summarize char
#' @param group - which factor level from groupChar is summarized
#' @param char - which characteristic is summarized from eSet
#' @export
#' @examples
#' returnSlotsUnique(eSet, "Patient.ID", "Patient7031", "SmokingStatus")

returnSlotsUnique <- function(eSet, groupChar, group, char){
  return(as.character(unique(eSet[[char]][eSet[[groupChar]]==group])))
}
