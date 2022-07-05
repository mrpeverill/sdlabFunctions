score_BQAQ<-function(meas) {
    #' Score the Buss-Perry Aggression Questionnaire (BQAQ)
    #'
    #' This is a scoring program for the BQAQ
    #'
    #'code{score_BQAQ} Returns cluster scoles for BQAQ subscales and total score.
    #'
    #' @param meas n×29 vector of reported responses
    #'
    #' @export

  # Check Dimensionality
  stopifnot(length(meas)==29)
    
  BUSSP_tot<-rowSums(meas)
  BUSSP_phys<-rowSums(meas[,1:9])
  BUSSP_verbal<-rowSums(meas[,10:14])
  BUSSP_anger<-rowSums(meas[,15:21])
  BUSSP_hostility<-rowSums(meas[,22:29])
  cbind(BUSSP_tot,BUSSP_phys,BUSSP_verbal,BUSSP_anger,BUSSP_hostility)
}
