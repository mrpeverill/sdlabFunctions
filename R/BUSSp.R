score_BUSSp<-function(meas) {
  stopifnot(length(meas)==29)
  BUSSP_tot<-rowSums(meas)
  BUSSP_phys<-rowSums(meas[,1:9])
  BUSSP_verbal<-rowSums(meas[,10:14])
  BUSSP_anger<-rowSums(meas[,15:21])
  BUSSP_hostility<-rowSums(meas[,22:29])
  cbind(BUSSP_tot,BUSSP_phys,BUSSP_verbal,BUSSP_anger,BUSSP_hostility)
}
