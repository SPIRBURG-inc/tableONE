numericSummary <- function(vec2Summarize, func1=mean, func2=sd, sigInts=2){
  charSummary <- as.character(round(func1(vec2Summarize), digits=sigInts))
  charSummary <- paste(charSummary, gsub("VAL2SUB", as.character(round(func2(vec2Summarize), digits=sigInts)),"(VAL2SUB)"))
  return(charSummary)
}