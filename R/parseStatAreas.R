#'
#'@title Convert statareas to centered lat/lons.
#'
#'@description Function that converts a vector of statareas to a 
#'list of lat, lon vectors corresponding to the centers of the statareas.
#'
#'@param x - vector of statareas to convert
#'
#'@return list with two vector elements: 'lon' and 'lat'
#'
#'@export
#'
parseStatAreas<-function(x){
    #convert stat areas to center lat, lon's
    x<-as.character(x);
    lon<--(100+as.numeric(substr(x,1,2)));
    lat<-as.numeric(substr(x,3,4))+as.numeric(substr(x,5,5))/6;
    return(list(lon=lon,lat=lat))
}