#' returnPVal - a helper function to return table1 p-values
#'  
#' This function returns the p-values for numeric and factor based comparisons between the groups
#' @param eSet - the eSet or data.frame sent to table1
#' @param groupChar - a factor column present in eSet by which to group samples and test char
#' @param char - which characteristic is summarized from eSet
#' @param numericTest Defaults to aov - changing this doesn't work yet
#' @param factorTest Defaults to fisher.test - changing this doesn't work yet
#' @export
#' @examples
#' returnNumSlots(eSet, "CancerStatus", "CancerPositive", "Age")
 
returnPVal <- function(eSet, groupChar, char, numericTest=aov, factorTest=fisher.test){
  
  if(is.factor(eSet[[char]])){
    pval <- format(factorTest(eSet[[char]], eSet[[groupChar]])$p.value, digits=3)
    pval <- t(t(c(pval, character(length(levels(eSet[[char]]))))))
  } else if(is.numeric(eSet[[char]])){
    #pval <- numericTest(eSet[[char]] ~ eSet[[groupChar]])$p.value
    pval <- format(summary(aov(eSet[[char]] ~ eSet[[groupChar]]))[[1]]["Pr(>F)"][1,1], digits=3)
  }
  return(pval)
}
