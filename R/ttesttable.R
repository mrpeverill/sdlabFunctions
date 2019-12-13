ttesttable<-function(varlist,groupvar) {
	#' Use t-tests to compare two groups across variables.
	#' 
	#' @param varlist A list or dataframe of values to compare
	#' @param groupvar A factor containing the group
	#' 
	#' Outputs a matrix
	#' 
	#' @export
	#' 
	tvect<-vector()
	pvect<-vector()
	for(i in 1:length(varlist)) {
		test<-t.test(varlist[[i]] ~ groupvar)
		tvect[i]<-test$statistic
		pvect[i]<-test$p.value
	}
	mat<-cbind(tvect,pvect)
	rownames(mat)<-names(varlist)
	colnames(mat)<-c("t","p")
	mat
}