sdcorr<-function(data,digits=2,labels=NULL,starAttach=FALSE) {
	#' Print a pretty correlation table.
	#' 
	#' This function prints a pretty correlation table.
	#'
	#' @param data a data frame with just numeric data.
	#' @param digits How many digits to print?
	#' @param labels Optional character vector of row labels. NULL uses variable names.
	#' @param starAttach If true, concatenate stars and correlations in character values.
	#' 
	#' @export
	#' 

	if(any(sapply(data,is.numeric)==FALSE)) {
		stop("Data must be numeric")
	}
	
	if (is.null(labels)) {
		rowlabels=names(data)
	} else {
		if(length(labels) == length(names(data))) {
			rowlabels=labels
		} else {
			stop("length of labels argument must equal data width")
		}
	}
	
	rawcorr<-Hmisc::rcorr(as.matrix(data))
	cors<-round(rawcorr[["r"]],digits) # round it
	ps<-rawcorr[["P"]]
	stars<-starPs(ps) # Get the stars
	dim(stars)<-dim(ps)
	
	#Delete the upper triangle from cors and ps
	cors[upper.tri(cors,diag=TRUE)]<-""
	stars[upper.tri(stars,diag=TRUE)]<-""
	
	if(starAttach) {
		cortable<-matrix(paste(cors,stars,sep=""),dim(cors)[1],dim(cors)[2])
		rownames(cortable)<-paste(1:ncol(cors),rowlabels,sep=". ")
		colnames(cortable)<-1:ncol(cors)
		cortable<-sub("^0+", "", cortable)
		return(cortable[,1:ncol(cortable)-1])
	} else {
		#We sample alternately from cors, then stars, then restore a matrix format
		cortable<-matrix(rbind(cors,stars),nrow=nrow(cors))
		rownames(cortable)<-paste(1:ncol(cors),rowlabels,sep=". ")
		colnames(cortable)<-c(sapply(1:ncol(cors),function(x) c(x,paste(x,".p"))))
		return(cortable[,1:(ncol(cortable)-2)])
	}
}
