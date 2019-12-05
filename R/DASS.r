score_DASS21<-function(meas) {
  #' This is a scoring program for the Depression and Anxiety Stress Scale-21.
  #' It accepts a data frame with 21 columns corresponding to responses
  #' on the measure (in order). 
  #'
  #' \code{score_DASS21} Returns three indices: depression, anxiety, and stress.
  
  #Length Check
  stopifnot(length(meas)==21)
  #Reverse Scoring
  DASS21_depr<-rowSums(meas[,c(3,5,10,13,16,17,21)])*2
  DASS21_anx<-rowSums(meas[,c(2,4,7,9,15,19,20)])*2
  DASS21_stress<-rowSums(meas[,c(1,6,8,11,12,14,18)])*2
  cbind(DASS21_depr,DASS21_anx,DASS21_stress)
}
