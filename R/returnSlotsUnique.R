returnSlotsUnique <- function(eSet, groupChar, group, char){
  return(as.character(unique(eSet[[char]][eSet[[groupChar]]==group])))
}
