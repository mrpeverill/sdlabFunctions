PIRcalc<- function(income,size,children,year=2018) {
  #' Calculate Poverty Income Ratio
  #'
  #' This function calculates the ratio of total family income given number of adults and children in a family.
  #' You must provide the year of the poverty thresholds you want to use, which are presently hard coded (it would be great to move them to a package data folder later).
  #'
  #' @income vector of total household income
  #' @size vector of total family size (including children)
  #' @children vector of number of children in the family.
  ThresholdList<-list("2018"=matrix(data=c(13064,NA,NA,NA,NA,NA,NA,NA,NA,
                                    16815,17308,NA,NA,NA,NA,NA,NA,NA,
                                    19642,20212,20231,NA,NA,NA,NA,NA,NA,
                                    25900,26324,25465,25554,NA,NA,NA,NA,NA,
                                    31234,31689,30718,29967,29509,NA,NA,NA,NA,
                                    35925,36068,35324,34612,33553,32925,NA,NA,NA,
                                    41336,41594,40705,40085,38929,37581,36102,NA,NA,
                                    46231,46640,45800,45064,44021,42696,41317,40967,NA,
                                    55613,55883,55140,54516,53491,52082,50807,50491,48546),nrow=9,ncol=9))
  size.c<-sapply(size,min,y=9)
  children.c<-sapply(children+1,min,y=9)
  try(if(is.null(ThresholdList[[as.character(year)]])) stop ("I don't have threshold data for that year"))
  try(if(any(children.c>size.c)) warning ("Some families have more children than people"))
  thresh<-ThresholdList[[as.character(year)]]
  sapply(1:length(income),function(x) income[x]/thresh[children.c[x],size.c[x]])
}
