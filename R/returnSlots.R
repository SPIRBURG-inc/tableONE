returnSlots <- function(eSet, groupChar, group, char, func=NULL){
  if(is.null(func)){
    if(is.factor(eSet[[char]])){
      return(returnFactorSlots(eSet, groupChar, group, char))
    } else if(is.numeric(eSet[[char]])){
      return(returnNumSlots(eSet, groupChar, group, char))
    }
  } else{
    return(func(eSet, groupChar, group, char))
  }
}