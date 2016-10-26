#' table1 - a function to generate a Table 1 figure
#'
#' This function allows you to generate a demographics table breaking down
#' the provided characteristics into the groups present in groupChar and 
#' optionally to perform tests to determine the differences between groups.
#' This table can be saved to CSV optionally as well.
#' @param eSet Either an expression set object or data.frame demographics table
#' @param groupChar The trait (a column name) from the eSet object; each factor level is assumed to be a group that will be represented in the table as a new column
#' @param characteristics The character vector of traits to summarize in the table
#' @param groupNames [optional] User can define the names to be printed in the table, replacing the levels in the groupChar factor
#' @param printN whether or not to add a row at the top of each column with the total number of samples present in that group
#' @param saveResultsCSV Defaults to FALSE; whether to save the table to a csv file
#' @param filename Defaults to tempTable1.csv; filename defining where to save the table
#' @param testGroupDiffs Defaults to FALSE; whether or not to test the group differences on each characteristic; factors tested by fisher and numerics by aov
#' @param factorTest - not implemented yet; Defaults to fisher.test; test used to determine group differences by factor characteristics
#' @param numericTest - not implemented yet; Defaults to aov; test used to determine group differences by numeric characteristics
#' @param userFuncs - not implemented yet
#' table1(eSet=expressionSet, groupChar="CancerStatus", characteristics=c("Smoking", "Sex", "Pack Years"))

table1 <- function(eSet, groupChar, characteristics,
                   groupNames=NULL, printN=T,
                   saveResultsCSV=F, filename="tempTable1.csv",
                   testGroupDiffs=F, factorTest=fisher.test, numericTest=aov,
                   userFuncs=NULL){
                  # treatFactors=NULL, treatNumerics=NULL,
                  # charTypes=NULL, charNames=NULL, UseSampleNames=F, byRow=F){
  
  # ERROR CHECKING
  # Make sure eSet is an expressionSet
  
  # Make sure groupChar is a factor in pData(eSet)
  
  # Make sure all characteristics are in names(pData(eSet))
  
  # is this plyr step necessary? gonna go with no for now...
  # which packages are required though? expressionSet used
  #require(plyr)
  # Remove unused factor levels
  #pData(eSet) <- droplevels(pData(eSet))
  
  # Define the groups
  groups <- levels(eSet[[groupChar]])
  # Define the number of columns
  numCols <- length(groups)
  if(testGroupDiffs){
    # add a column if groups will be compared
    numCols <- numCols + 1
  }
  # Define the number of rows needed in the table
  numRows <- tableRowNum(eSet, characteristics)
  # add a row to the table if N (number samples) will be printed per group
  if(printN){
    numRows <- numRows + 1
  }
  
  # Create the empty table
  tableONE <- data.frame(matrix(nrow=numRows, ncol=numCols))
  
  # Name the columns of the table
  names(tableONE) <- groups
  
  # add a column name for p-values when desired
  if(testGroupDiffs){
    names(tableONE)[numCols] <- "p-val"
  }
  
  # Name the rows of the table (from the characteristics and their levels where applicable)
  rownames(tableONE) <- tableRowNames(eSet, characteristics, printN)
  
  # For each group define the relevant characteristics
  for(i in groups){
    
    rowInd <- 1
    
    # fill in the N value in the first row
    if(printN){
      tableONE[[i]][rowInd] <- gsub("VALUE", sum(eSet[[groupChar]]==i), "(n = VALUE)")
      rowInd <- rowInd + 1
    }
    for(j in 1:length(characteristics)){
      
      char <- characteristics[j]
      
      # Return vector to fill in the next available slots of the table
      nextSlots <- returnSlots(eSet, groupChar, i, char)
      
      # Fill the data into the table
      tableONE[[i]][seq(from=rowInd, to=rowInd+length(nextSlots)-1)] <- nextSlots
      rowInd <- rowInd + length(nextSlots)
    } 
  }
  
  # For each characteristic run the appropriate statistical test and return the p-value
  if(testGroupDiffs){
    rowInd <- 1
    if(printN){
      tableONE[["p-val"]][1] <- ""
      rowInd <- rowInd + 1
    }
    
    for(j in 1:length(characteristics)){
      char <- characteristics[j]
      
      # define the p-vals for the characteristic based on its class
      pvals <- returnPVal(eSet, groupChar, char)
      tableONE[["p-val"]][seq(from=rowInd, to=rowInd+length(pvals)-1)] <- pvals
      rowInd <- rowInd + length(pvals)
    }
  }
  
  # Rename the columns if desired
  if(!is.null(groupNames)){
    if(length(groupNames)==length(groups)){
      names(tableONE) <- groupNames
    } else{
      warning("groupNames is provided but does not have the appropriate number of names")
    }
  }
  
  if(saveResultsCSV){
    # write out a table to csv
    write.table(temp, quote=F, sep=",", file=filename)
  }
  return(tableONE)
}
  

# tableRowNum <- function(eSet, characteristics){
#   numRows <- 0
#   for(i in characteristics){
#     if(class(eSet[[i]])=="factor"){
#       numRows <- numRows + 1 + length(levels(eSet[[i]]))
#     } else if (class(eSet[[i]])=="integer" | class(eSet[[i]])=="numeric"){
#       numRows <- numRows + 1
#     } 
#   }
#   return(numRows)
# }
# 
# 
# tableRowNames <- function(eSet, characteristics, printN){
#   tableNames <- character()
#   if(printN){
#     tableNames <- rbind(tableNames, "n")
#   }
#   for(i in characteristics){
#     tableNames <- rbind(tableNames, i)
#     if(class(eSet[[i]])=="factor"){
#       for(j in levels(eSet[[i]])){
#         tableNames <- rbind(tableNames, j)
#       }
#     } 
#   }
#   return(tableNames)
# }
# 
# 
# # Numeric summary returns the mean and sd (or the supplied arguments) for a given numeric vector
# numericSummary <- function(vec2Summarize, func1=mean, func2=sd, sigInts=2){
#   charSummary <- as.character(round(func1(vec2Summarize), digits=sigInts))
#   charSummary <- paste(charSummary, gsub("VAL2SUB", as.character(round(func2(vec2Summarize), digits=sigInts)),"(VAL2SUB)"))
#   return(charSummary)
# }
# 
# 
# returnNumSlots <- function(eSet, groupChar, group, char){
#   return(numericSummary(eSet[[char]][eSet[[groupChar]]==group]))
# }
# 
# 
# returnFactorSlots <- function(eSet, groupChar, group, char){
#   return(t(t(c("", summary(eSet[[char]][eSet[[groupChar]]==group])))))
# }
# 
# 
# returnSlots <- function(eSet, groupChar, group, char, func=NULL){
#   if(is.null(func)){
#     if(is.factor(eSet[[char]])){
#       return(returnFactorSlots(eSet, groupChar, group, char))
#     } else if(is.numeric(eSet[[char]])){
#       return(returnNumSlots(eSet, groupChar, group, char))
#     }
#   } else{
#     return(func(eSet, groupChar, group, char))
#   }
# }
# 
# # Starting to much about with other kinds of slots... how to best do this though
# returnSlotsUnique <- function(eSet, groupChar, group, char){
#   return(as.character(unique(eSet[[char]][eSet[[groupChar]]==group])))
# }
# 
# 
# returnPVal <- function(eSet, groupChar, char, numericTest=aov, factorTest=fisher.test){
#   
#   if(is.factor(eSet[[char]])){
#     pval <- format(factorTest(eSet[[char]], eSet[[groupChar]])$p.value, digits=3)
#     pval <- t(t(c(pval, character(length(levels(eSet[[char]]))))))
#   } else if(is.numeric(eSet[[char]])){
#     #pval <- numericTest(eSet[[char]] ~ eSet[[groupChar]])$p.value
#     pval <- format(summary(aov(eSet[[char]] ~ eSet[[groupChar]]))[[1]]["Pr(>F)"][1,1], digits=3)
#   }
#   return(pval)
# }





# Some old code used at beginning for table1 functionality


# Define the patients
#  table1 <- data.frame(Patient=levels(target$Patient.ID))

# Define the groups
#  table1$Group <- target$group[match(table1$Patient, target$Patient.ID)]

# Define the number of samples
#  table1$Samples <- table(target$Patient.ID)

# Define sex
#  table1$Sex <- target$sex[match(table1$Patient, target$Patient.ID)]

# Define age
#  table1$Age <- target$age[match(table1$Patient, target$Patient.ID)]

# Define pack years
#  table1$PackYears <- target$smk_PY[match(table1$Patient, target$Patient.ID)]

# Define smoking status
#  table1$Smoker <- target$smk[match(table1$Patient, target$Patient.ID)]

# Define the RIN Range
#  table1$RINRange <- apply(tapply(target$RIN, target$Patient.ID, range), 1, function(x){paste(as.character(x[[1]]), collapse="-")})

# Define the LM Mean +/- SD ("\u00b1" is plus-minus symbol)
#  mn1 <-  round(tapply(target$Lm, target$Patient.ID, mean), 0)
#  sd1 <-  round(tapply(target$Lm, target$Patient.ID, sd), 0)
#  table1$LMSummary <- paste(mn1, " \u00b1 ", sd1)

# Define the LM Range
#  table1$LMRange <- tapply(target$Lm, target$Patient.ID, function(x){paste(round(range(x), 0), collapse="-")})

#  table1 <- arrange(table1, desc(Group), Patient)
#  if(saveResults){
#    write.csv(table1, file=fNameGen("table1.csv"))
#  }

#  table1

# need to add gender, smoking, pack years, whatever else we have
#}
