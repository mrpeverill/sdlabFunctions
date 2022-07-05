score_CDI2 <- function(meas,component=TRUE,nacorrect=FALSE,navalue=1,nathresh=3) {
	#' This is a scoring program for the CDI version 2.
	#'
	#' \code{score_CDI2} Returns (raw) total symptom score for the CDI.
	#'
	#' @param data n×28 vector of item responses
	#' @param component should the component scores be returned?
	#' @param navalue What should we inpute for missing item scores? 0-2 or NULL. NA will retain NA through scoring. 
	#' @param nathresh How many items can be imputed before the subject's score is invalidated?
	#' 
	#' This is a scoring program for the 28 item Childhood Depression Inventory.
	#' It accepts a data frame with 28 columns corresponding to responses
	#' on the measure (in order). It returns raw scores only.
	#'
	
	# Error Handling
	inputchecks<-checkmate::makeAssertCollection()
	checkmate::assertChoice(navalue,
							c(0,1,2),
							null.ok=TRUE,
							add = inputchecks
							)
	checkmate::assertVector(meas,
							min.len=28,
							max.len=28,
							add=inputchecks)
	checkmate::reportAssertions(inputchecks)
	
	#Reverse Scoring
	reverse<-c(2,6,7,9,10,12,14,15,17,20,23,24,26,27)
	meas[,reverse]<-2-meas[,reverse]
	
	# Missing Data Handling
	if(nacorrect) {
		if(navalue %in% c(0,1,2)) {
			meas$nmiss<-rowSums(is.na(meas))
			correctable<-meas$nmiss<=nathresh
			meas[correctable,]<-replace(meas[correctable,],
										is.na(meas[correctable,]),
										navalue) #Yes dplyr would be faster
		}
		if(navalue=="fiml") {
			
			
		}
	}
	
	# Ref: Manual pg 32
	Aitems<-c(1,9,10,15,16,17,18,26,27)
	Bitems<-c(2,6,7,8,13,24)
	Citems<-c(3,4,12,14,20,22,23,28)
	Ditems<-c(5,11,19,21,25)
	
	results<-data.frame(CDI.MoodPhysical=rowSums(meas[,Aitems]),
						CDI.SelfEsteem=rowSums(meas[,Bitems]),
						CDI.Ineffective=rowSums(meas[,Citems]),
						CDI.Interpersonal=rowSums(meas[,Ditems]))
	
	results$CDI.EmoProblems<-results$CDI.MoodPhysical+results$CDI.SelfEsteem
	results$CDI.FunctionalProblems<-results$CDI.Ineffective+results$CDI.Interpersonal
	
	results$CDI.tot<-results$CDI.EmoProblems+results$CDI.FunctionalProblems
	
	# What are we returning?
	returnindices<-c("CDI.tot")
	if(component) {
		returnindices<-c(returnindices,
						 "CDI.MoodPhysical",
						 "CDI.SelfEsteem",
						 "CDI.Ineffective",
						 "CDI.Interpersonal",
						 "CDI.EmoProblems",
						 "CDI.FunctionalProblems")
		}
		
	return(results[,returnindices])
}

score_CDI1 <- function(meas,component=TRUE,nacorrect=FALSE,navalue=1,nathresh=3) {
	#' This is a scoring program for the CDI version 1.
	#'
	#' \code{score_CDI2} Returns (raw) total symptom score for the CDI.
	#'
	#' @param data n×28 vector of item responses
	#' @param navalue What should we inpute for missing item scores? 0-2 or NULL. 'fiml' will attempt to use fiml to estimate item scores. NA will retain NA through scoring. 
	#' @param nathresh How many items can be imputed before the subject's score is invalidated?
	#' 
	#' This is a scoring program for the 26 item Childhood Depression Inventory.
	#' It accepts a data frame with 26 columns corresponding to responses
	#' on the measure (in order).
	#'
	
	# Error Handling
	inputchecks<-checkmate::makeAssertCollection()
	checkmate::assertChoice(navalue,
							c(0,1,2),
							null.ok=TRUE,
							add = inputchecks
	)
	checkmate::assertVector(meas,
							min.len=26,
							max.len=26,
							add=inputchecks)
	checkmate::reportAssertions(inputchecks)
	
	#Reverse Scoring
	reverse<-c(2,5,7,8,9,10,12,14,15,17,20,23,24)
	meas[,reverse]<-2-meas[,reverse]
	
	# Missing Data Handling
	if(nacorrect) {
		if(navalue %in% c(0,1,2)) {
			meas$nmiss<-rowSums(is.na(meas))
			correctable<-meas$nmiss<=nathresh
			meas[correctable,]<-replace(meas[correctable,],
										is.na(meas[correctable,]),
										navalue) #Yes dplyr would be faster
		}
		if(navalue=="fiml") {
			
			
		}
	}
	data.frame(CDI.tot=rowSums(meas))
}
