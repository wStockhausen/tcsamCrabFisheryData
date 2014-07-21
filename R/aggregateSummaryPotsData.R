#'
#'@title Aggregate data from ADF&G "all pots" (SummaryPots) data files.
#'
#'@description Function to aggregate (and, optionally, map) data from ADF&G "all pots" (SummaryPots) data files.
#'
#'@details plotMaps.ObservedSummaryPots(...) is used to map the aggregated data if plotMaps is TRUE.  
#'
#'@param tbl - dataframe derived from "all pots" (ObservedSummaryPots) csv file, or csv file itself
#'@param plotMaps - flag (T/F) to plot maps derived from the dataframe
#'@param byQuarter - flag to plot maps by fishery quarter
#'@param byFishery - flag to plot maps by fishery type
#'@param ... - additional inputs to plotMaps.ObservedSummaryPots(tbl,byQuarter,byFishery,...)
#'
#'@return dataframe
#' 
#'
#'@import sqldf
#'@importFrom wtsUtilties getCSV
#'
#'@export
#' 
aggregateSummaryPotsData<-function(tbl=NULL,
                                   byQuarter=FALSE,
                                   byFishery=FALSE,
                                   plotMaps=TRUE,
                                   ...){
    
    #create 'all pot' (ObservedSummaryPots) data table (if not an input)
    if (!is.data.frame(tbl)){
        cat("Reading ADF&G 'all pots' (SummaryPots) csv file.\n")
        if (is.null(tbl)) {
            tbl<-wtsUtilities::getCSV(caption='Select ADF&G "all pots" (SummaryPots) csv file');
            if (is.null(tbl)) return(NULL);
        } else {
            tbl<-read.csv(tbl,stringsAsFactors=FALSE);
        }
        cat("Done reading input csv file.\n")
    }
    
    #add month and quarter dataframe
    names(tbl)<-tolower(names(tbl));
    tbl[['month']]<-wtsUtilities::parseMonths(tbl$date,format='MM/DD/YY')
    tbl[['qrtr']]<-getCrabFisheryQuarterFromMonth(tbl$month);
    
    #calculate number of observed pots by fishery type, year, quarter, and statarea
    qry<-"select
            fisheryyear,
            qrtr as fisheryquarter,
            fisherytype,
            statarea,
            count(distinct adfg) as vessels,
            count(spn) as pots,
            sum(female) as female,
            sum(sublegal) as sublegal,
            sum(legalnr) as legalnr,
            sum(legalret) as legalret
        from
            tbl
        group by
            fisheryyear,qrtr,fisherytype,statarea
        order by
            fisheryyear,fisheryquarter,fisherytype,statarea;";
    tbl1<-sqldf(qry);
    
    lls<-parseStatAreas(tbl1$statarea);
    tbl1[["lon"]]<-lls$lon;
    tbl1[["lat"]]<-lls$lat;
    
    idx<-!is.na(tbl1$statarea);
    tbl1<-tbl1[idx,];
    
    idx<-(tbl1$statarea==0);
    tbl1$lat[idx]<-NA;
    tbl1$lon[idx]<-NA;
    
    idx<-(tbl1$statarea==-9);
    tbl1$lat[idx]<-NA;
    tbl1$lon[idx]<-NA;
    
    tbl1a<-tbl1[(!is.na(tbl1$lat)),];
    
    fcstr<-"fisheryyear,statarea,lat,lon";
    if (byQuarter) {
      fcstr<-paste(fcstr,"fisheryquarter",sep=",");
    }    
    if (byFishery) {
      fcstr<-paste(fcstr,"fisherytype",sep=",");
    }
    qry<-"select
            &&fcstr,
            sum(vessels) as vessels,
            sum(pots) as pots,
            sum(female) as female,
            sum(sublegal) as sublegal,
            sum(legalnr) as legalnr,
            sum(legalret) as legalret,
            sum(legalnr+legalret) as legal
        from
            tbl1a
        group by
            &&fcstr;";
    qry<-gsub("&&fcstr",fcstr,qry)
    tbl2<-sqldf(qry);
    if (!byQuarter) tbl2[["fisheryquarter"]]<-'';
    if (!byFishery) tbl2[["fisherytype"]]<-'';
    cat("tbl2:",'\n')
    print(tbl2);
    
    if (plotMaps){
        plotMaps.SummaryPotsData(tbl=tbl2,byQuarter=byQuarter,byFishery=byFishery,...)
    }
    
    return(tbl2);
}

#tbl2<-aggregateSummaryPotsData(plotMaps=TRUE)
