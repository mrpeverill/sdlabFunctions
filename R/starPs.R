starPs<-function(p,s3=.001,s2=.01,s1=.05) {
	#' This function formats '***' style significance strings for a column of p values.
	#'
	#' @param p vector of p values.
	#' @param s3 Smallest, ***, significance threshold.
	#' @param s2 Middle, **, significance threshold.
	#' @param s1 Largest, *, significance threshold.
	#' 
	#' 
	sapply(p,function(x) if(is.na(x)) {
		NA
	} else if(x<=s3) {
		'***'
	} else if(x<=s2) {
		'**'
	} else if(x<=s1) {
		'*'
	} else {""})
}