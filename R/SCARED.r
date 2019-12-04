score_SCARED<-function(meas,nathresh=3) {
  #' This is a scoring program for the SCARED
  #'
  #' \code{score_SCARED} Returns total symptom score for the SCARED.
  #'
  #' @param meas data frame or matrix with 41 columns representing SCARED reesponses.
  #' @param nathresh How many skipped answers are allowed before the subject is marked NA? 0 for no NA handling.

  stopifnot(length(meas)==41)
  Sums<-rowSums(meas,na.rm=TRUE)
  if(!is.null(nathresh)){
    missingness<-rowSums(is.na(meas))>=nathresh
    if(any(missingness)) {
      warning("At least one participant didn't have enough responses")
    }
    Sums[missingness]<-NA
  }
  Sums
}
