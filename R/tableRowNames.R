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