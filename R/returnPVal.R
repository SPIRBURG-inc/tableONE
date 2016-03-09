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
