score_DAST<-function(meas) {
  #' This is a scoring program for the 28 item Drug Abuse Screening Test.
  #' It accepts a data frame with 28 columns corresponding to responses
  #' on the measure (in order).
  #'
  #' \code{score_DAST} Returns a total symptom score.
  
  reverse<-c(4,5,7)
  #Length Check
  stopifnot(length(meas)==28)
  #Reverse Scoring
  meas[,reverse]<-1-meas[,reverse]
  rowSums(meas)
}
