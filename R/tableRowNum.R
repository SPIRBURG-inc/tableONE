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
