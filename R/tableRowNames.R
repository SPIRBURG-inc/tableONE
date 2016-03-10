#' tableRowNames - a helper function to return the row names for table1
#'  
#' This function returns a character vector that can be placed into rownames(table1) and that provides information about the table's contents
#' @param eSet - the eSet or data.frame sent to table1
#' @param characteristics - which columns from eSet will need names
#' @param printN - whether N per group will be printed in the table
#' @export
#' @examples
#' tableRowNames(eSet, c("SmokingStatus", "Age", "PackYears"), TRUE)

tableRowNames <- function(eSet, characteristics, printN){
  tableNames <- character()
  if(printN){
    tableNames <- rbind(tableNames, "n")
  }
  for(i in characteristics){
    tableNames <- rbind(tableNames, i)
    if(class(eSet[[i]])=="factor"){
      for(j in levels(eSet[[i]])){
        tableNames <- rbind(tableNames, j)
      }
    } 
  }
  return(tableNames)
}