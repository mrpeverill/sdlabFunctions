score_CTQ <- function(data,thresh.scores=TRUE) {
  #' This is a scoring program for the CTQ.
  #'
  #' \code{score_CTQ} Returns cluster scores for CTQ subscales, along with dichotomous threshold variables.
  #'
  #' @param data n×28 vector of reported responses.
  #' @param thresh.scores boolean - program returns threshold scores if TRUE.

  #Check Dimensionality
  if (dim(haka)[2] != 28) stop("data has wrong length; 28 items must be specified")
  data<-data-1
  data[,c(2,5,7,13,19,26,28)]<-(data[,c(2,5,7,13,19,26,28)]-4)*-1
  CTQ_physneg = rowSums(data[,c(1,2,4,6,26)])
  CTQ_emoneg = rowSums(data[,c(5,7,13,19,28)])
  CTQ_emoab = rowSums(data[,c(3, 8, 14, 18, 25)])
  CTQ_physab = rowSums(data[,c(9, 11, 12, 15, 17)])
  CTQ_sexab = rowSums(data[,c(20, 21, 23, 24, 27)])
  results<-data.frame(physneg_sev=CTQ_physneg,
                      emoneg_sev=CTQ_emoneg,
                      emoab_sev=CTQ_emoab,
                      physab_sev=CTQ_physab,
                      sexab_sev=CTQ_sexab,
                      physab_thresh=CTQ_physab>7,
                      sexab_thresh=CTQ_sexab>7,
                      emoab_thresh=CTQ_emoab>10)
  if(thresh.scores) {
    en=7} else {
      en=5
    }
  results[,1:en]
}
