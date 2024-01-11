calc_revzscore<-function(z,v) {
	#' \code{calc_zscore} Returns the raw value of a z score given the non-standardized vector v.
	#'
	#' @param z a numeric value
	#' @param v a numeric vector
	#'
	z*sd(v,na.rm = TRUE)+mean(v,na.rm = TRUE)	
}

calc_zscore<-function(z,v) {
	#' \code{calc_zscore} Returns the z score of a raw value given the non-standardized vector v.
	#'
	#' @param z a numeric value
	#' @param v a numeric vector
	#'
	(z-mean(v,na.rm = TRUE))/sd(v,na.rm = TRUE)
}