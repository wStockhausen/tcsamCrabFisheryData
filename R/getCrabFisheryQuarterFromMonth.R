#'
#'@title Convert month(s) to quarters by crab fishery year (starting July 1).
#'
#'@description Function that converts a vector of months to a vector of 
#'quarters by crab fishery year (i.e., starting July 1).
#'
#'@param x - vector of months to convert
#'
#'@return vector of crab fishery quarters
#'
#'@export
#'
getCrabFisheryQuarterFromMonth<-function(x){
    #convert 1-3 to 3rd qtr, 4-6 to 4th qtr, 7-9 to 1st qtr, 10-12 to 2nd qtr
    xp<-(((as.numeric(x)-1)%/%3)+2)%%4+1;    
    return(xp)
}