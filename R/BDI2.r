score_BDI2<-function(meas) {
	#' Score the Beck Depression Inventory (v2)
	#' 
	#' This is a scoring program for the Beck Depression Inventory (v2).
	#' It accepts a data frame with 21 columns corresponding to responses
	#' on the measure.
	#'
	#' \code{score_BDI2} Returns a symptom total.
	#' 
	#' @export
	
  stopifnot(length(meas)==21)
  rowSums(meas)
}
