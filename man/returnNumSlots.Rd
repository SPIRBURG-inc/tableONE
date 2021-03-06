% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/returnNumSlots.R
\name{returnNumSlots}
\alias{returnNumSlots}
\title{returnNumSlots - a helper function to return character summary of a numeric based on the groupChar
 
This function returns the text from numericSummary; the returned value is formatted such that it can be directly placed in the table 1 column}
\usage{
returnNumSlots(eSet, groupChar, group, char)
}
\arguments{
\item{eSet}{- the eSet or data.frame sent to table1}

\item{groupChar}{- a factor column present in eSet by which to group samples and summarize char}

\item{group}{- which factor level from groupChar is summarized}

\item{char}{- which characteristic is summarized from eSet}
}
\description{
returnNumSlots - a helper function to return character summary of a numeric based on the groupChar
 
This function returns the text from numericSummary; the returned value is formatted such that it can be directly placed in the table 1 column
}
\examples{
returnNumSlots(eSet, "CancerStatus", "CancerPositive", "Age")
}

