#'
#'@title Convert lat/lons to crab fishery stat areas.
#'
#'@description Function that converts vectors of lat/lons to crab fishery stat areas.
#'
#'@param lats - vector of lats to convert
#'@param lats - vector of lons to convert
#'
#'@return vector of crab fishery stat areas
#'
#'@export
#'
createStatAreas<-function(lats=NULL,lons=NULL){
    statareas<-10000*floor(-lons-100)+100*floor(lats)+30*round(lats%%1);
    return(statareas)
}
