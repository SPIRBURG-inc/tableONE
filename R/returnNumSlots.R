returnNumSlots <- function(eSet, groupChar, group, char){
  return(numericSummary(eSet[[char]][eSet[[groupChar]]==group]))
}