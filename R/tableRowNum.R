#' tableRowNum - a helper function to determine the number of rows needed in table1
#'  
#' This function returns the number of rows that will be needed for table 1 based on the variables that will be summarized
#' @param eSet - the eSet or data.frame sent to table1
#' @param characteristics - which characteristics are summarized from eSet
#' @export
#' @examples
#' tableRowNum(eSet, c("CancerStatus", "SmokingStatus", "PackYears"))

tableRowNum <- function(eSet, characteristics){
  numRows <- 0
  for(i in characteristics){
    if(class(eSet[[i]])=="factor"){
      numRows <- numRows + 1 + length(levels(eSet[[i]]))
    } else if (class(eSet[[i]])=="integer" | class(eSet[[i]])=="numeric"){
      numRows <- numRows + 1
    } 
  }
  return(numRows)
}
