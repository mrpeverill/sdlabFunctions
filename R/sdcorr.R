sdcorr<-function(data,digits=2) {
	#' Print a pretty correlation table.
	#' 
	#' This function prints a pretty correlation table.
	#'
	#' @param data a data frame with just numeric data.
	#' @param digits How many digits to print?
	#' 
	#' @export
	#' 

	if(any(sapply(data,is.numeric)==FALSE)) {
		stop("Data must be numeric")
	}
	
	rawcorr<-Hmisc::rcorr(as.matrix(data))
	cors<-round(rawcorr[["r"]],digits) # round it
	ps<-rawcorr[["P"]]
	stars<-starPs(ps) # Get the stars
	dim(stars)<-dim(ps)
	
	#Delete the upper triangle from cors and ps
	cors[upper.tri(cors,diag=TRUE)]<-""
	stars[upper.tri(stars,diag=TRUE)]<-""
	
	#We sample alternately from cors, then stars, then restore a matrix format
	cortable<-matrix(rbind(cors,stars),nrow=nrow(cors))
	rownames(cortable)<-paste(1:ncol(cors),names(data),sep=". ")
	colnames(cortable)<-c(sapply(1:ncol(cors),function(x) c(x,paste(x,".p"))))
	cortable[,1:(ncol(cortable)-2)]
}