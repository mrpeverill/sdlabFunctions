score_CAPS <- function(crita,life_frequency,curr_frequency,life_intens=0,curr_intens=0,nacorrect=FALSE,correct.errors=FALSE,cluster.scores=FALSE) {
  #' This is a scoring program for the CAPS-V, using rules from our MT dataset.
  #'
  #' \code{score_CAPS} Returns current and lifetime PTSD symptom counts and
  #' diagnosis codes given appropriate input. Typically input is given as
  #' column subsets of a larger dataframe. All inputs must be numeric.
  #'
  #' @param crita n×1 vector of criterion A (qualifying trauma)
  #' @param life_frequency n×25 vector of reported lifetime frequency.
  #' @param curr_frequency n×25 vector of reported current frequency.
  #' @param life_intensity n×20 vector of reported lifetime intensity -
  #' defaults to 0 for cases where intensity not reported (eg parent
  #' report).
  #' @param curr_intensity n×20 vector of reported current intensity -
  #' defaults to 0 for cases where intensity not reported (eg parent
  #' report).
  #' @param skipped.is.na are items which have been properly skipped
  #' (eg. all items following absence of crit A) coded as NA? If so,
  #' the program will substitute appropriately with zeroes before
  #' scoring. Otherwise it will attempt to score with standard R
  #' handling of missing values.
  #' @param correct.errors should the program correct common
  #' interview errors. Currently this means if lifetime score for a
  #' symtom is lower than current, lifetime will be overwritten.
  #' @param cluster.scores should the program print cluster scores.

  #Check Dimensionality
  if (dim(life_frequency)[2] != 25) stop("life_freq has wrong length; 25 items must be specified")
  if (dim(curr_frequency)[2] != 25) stop("curr_freq has wrong length; 25 items must be specified")
  if ((!identical(0,life_intens)) && (dim(life_intens)[2] != 20)) stop("life_intens has wrong length; 20 items must be specified")
  if ((!identical(0,curr_intens)) && (dim(life_intens)[2] != 20)) stop("life_intens has wrong length; 20 items must be specified")

  #Data must be numeric
  if(!all(sapply(c(life_frequency,curr_frequency,life_intens,curr_intens),is.numeric))) stop("all values must be numeric")

  #f1 is a yes-no question and we need to be sure it gets coded as 0-1 and not 1-2.
  if (max(rbind(setNames(life_frequency[21],"x"),
                setNames(curr_frequency[21],"x")),na.rm=TRUE)>1) stop("item 21 (f_1) should be coded 0-1 or FALSE-TRUE")

  lf<-as.matrix(life_frequency)
  cf<-as.matrix(curr_frequency)
  li<-as.matrix(life_intens)
  ci<-as.matrix(curr_intens)

if(nacorrect==TRUE) { #typically NA scores for symptoms are because they are ruled out.
  lf[is.na(lf)]<-0
  cf[is.na(cf)]<-0
  li[is.na(li)]<-0
  ci[is.na(ci)]<-0
  }

  if(correct.errors) { # lifetime is always at least as high as current.
    freqcorrset<- cf>lf & !is.na(cf) & !is.na(lf)
    intenscorrset<- ci>li & !is.na(ci) & !is.na(li)
    lf[freqcorrset]<-cf[freqcorrset]
    li[intenscorrset]<-ci[intenscorrset]
  } else {
    if ( sum(cf>lf,na.rm=TRUE) > 0 ) warning(c(sum(cf>lf,na.rm=TRUE)," lifetime frequency scores are lower than current score."))
    if ( sum(ci>li,na.rm=TRUE) > 0 ) warning(c(sum(ci>li,na.rm=TRUE)," lifetime intensity scores are lower than current score."))
  }

  if(identical(li,as.matrix(0))) {
  ls<-lf
  } else {
    lpad<-matrix(nrow=nrow(li),ncol=5) # We need to make the intensity matrices the same size as the others or weird things will happen.
    lpad[,]<-0
    ls<-pmax(lf,cbind(li,lpad))
  }

  if(identical(ci,as.matrix(0))) {
    cs<-cf
  } else {
    cpad<-matrix(nrow=nrow(ci),ncol=5) # We need to make the intensity matrices the same size as the others or weird things will happen.
    cpad[,]<-0
    cs<-pmax(cf,cbind(ci,cpad))
  }

  capsA<-crita
  capsBl<-rowSums(ls[,1:5]>1)
  capsBc<-rowSums(cs[,1:5]>1)
  capsCl<-rowSums(ls[,6:7]>1)
  capsCc<-rowSums(cs[,6:7]>1)
  capsDl<-rowSums(ls[,8:14]>1)
  capsDc<-rowSums(cs[,8:14]>1)
  capsEl<-rowSums(ls[,15:20]>1)
  capsEc<-rowSums(cs[,15:20]>1)
  capsFl<-ls[,21]>0
  capsFc<-cs[,21]>0
  capsGl<-rowSums(ls[,22:25]>0)
  capsGc<-rowSums(cs[,22:25]>0)

  sxl<-rowSums(cbind(capsBl,capsCl,capsDl,capsEl))
  dxl<-capsA==1 & capsBl>0 & capsCl>0 & capsDl>1 & capsEl>1 & capsFl==TRUE & capsGl>0
  dxl[is.na(capsA)]<-NA # If we don't have caps A, the subject is probably missing and we should not return FALSE for diagnosis.

  sxc<-rowSums(cbind(capsBc,capsCc,capsDc,capsEc))
  dxc<-capsA==1 & capsBc>0 & capsCc>0 & capsDc>1 & capsEc>1 & capsFc==TRUE & capsGc>0
  dxc[is.na(capsA)]<-NA # If we don't have caps A, the subject is probably missing and we should not return FALSE for diagnosis.
  if(cluster.scores) {clusters<-cbind(capsA,capsBl,capsBc,capsCl,capsCc,capsDl,capsDc,capsEl,capsEc,capsFl,capsFc,capsGl,capsGc)} else {clusters<-vector()}
  return(data.frame(cbind(clusters,sxl,dxl,sxc,dxc)))
}