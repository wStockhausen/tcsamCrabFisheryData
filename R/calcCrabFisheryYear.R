#'
#'@title Convert dates to crab fishery year (starting July 1).
#'
#'@description Function that converts a vector of dates to crab fishery year (i.e., starting July 1).
#'
#'@param dates - vector of dates to convert
#'@param years - vector of calendar years to convert (must supply 'month', as well)
#'@param months - vector of months (1-12) to use to convert calendar years
#'@param format - format for date strings
#'
#'@return vector of crab fishery years
#'
#'@importFrom wtsUtilities parseYears
#'@importFrom wtsUtilities parseMonths
#'@export
#'
calcCrabFisheryYear<-function(dates=NULL,years=NULL,months=NULL,format='MM/DD/YYYY'){
    if (!is.null(dates)){
        years<-wtsUtilities::parseYears(dates,format=format);
        months<-wtsUtilities::parseMonths(dates,format=format);
    }
    crabyears<-years*(months>6)+(years-1)*(months<7);
    return(crabyears)
}
