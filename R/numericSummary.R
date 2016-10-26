#' numericSummary provides the table text for summarizing a numeric vector
#'
#' This function allows text summarization of a numeric vector. The summary functions can be specified as can the number of significant digits returned
#' @param vec2Summarize - the numeric vector to summarize
#' @param func1 Defaults to mean; any function working on a numeric vector can be used
#' @param func2 Defaults to sd; any function working on a numeric vector can be used
#' @param sigInts Defaults to 2; the number of significant integers to print in the returned character
#' @export
#' @examples
#' numericSummary(1:10)

numericSummary <- function(vec2Summarize, func1=mean, func2=sd, sigInts=2){
  charSummary <- as.character(round(func1(vec2Summarize), digits=sigInts))
  charSummary <- paste(charSummary, gsub("VAL2SUB", as.character(round(func2(vec2Summarize), digits=sigInts)),"(VAL2SUB)"))
  return(charSummary)
}