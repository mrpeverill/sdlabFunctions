score_BENTHIN<-function(meas) {
  #' This is a scoring program for the Benthin Risk Perception Scale.
  #' It accepts a data frame with 54 columns corresponding to responses
  #' on the measure (in order).
  #'
  #' \code{score_BDI2} Returns four indices: fear, risk, benefit, and severity.
  
  #Length Check
  stopifnot(length(meas)==54)
  #Reverse Scoring
  RISK_fear<-rowMeans(meas[,seq(1,54,6)],na.rm=TRUE)
  RISK_risk<-rowMeans(meas[,seq(2,54,6)],na.rm=TRUE)
  RISK_benefit<-rowMeans(meas[,seq(3,54,6)],na.rm=TRUE)
  RISK_severity<-rowMeans(meas[,seq(4,54,6)],na.rm=TRUE)
  cbind(RISK_fear,RISK_risk,RISK_benefit,RISK_severity)
}
