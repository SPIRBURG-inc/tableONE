returnFactorSlots <- function(eSet, groupChar, group, char){
  return(t(t(c("", summary(eSet[[char]][eSet[[groupChar]]==group])))))
}