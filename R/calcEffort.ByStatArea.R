#'
#'@title Calculate observer effort (pots) by stat area from ADF&G detailed "measure" pot data.
#'
#'@description Function to calculate observer effort by stat area 
#'from ADF&G detailed "measure" pot data.
#'
#'@details Function to calculate observer effort by stat area 
#'from ADF&G detail (individual crab measurements) or summary (numbers by crab type) pot data.
#'
#'@param tbl - dataframe derived from ADF&G observed pot data.
#'@param out.csv - filename base for output csv file
#'
#'@return dataframe with the following columns:
#' * fisheryyear - crab fishery year (YYYY: July YYYY-June YYYY+1)
#' * fisheryquarter - quarter for crab fishery year (July-June)
#' * fisherytype - fishery type
#' * statarea - ADFG stat area
#' * vessels - number of observed vessels
#' * trips - number of observer trips
#' * pots - number of pots sampled
#'
#'@import sqldf
#'@importFrom wtsUtilities getCSV
#'
#'@export
#' 
calcEffort.ByStatArea<-function(tbl=NULL,
                                out.csv=NULL){
  
    #create detailed 'measure' pots data table (if not an input)
    if (!is.data.frame(tbl)){
        cat("Read ADF&G 'detail' or 'summary' pots csv file.\n")
        if (is.null(tbl)) {
            tbl<-wtsUtilities::selectFile(caption='Select ADF&G "detail" or "summary" pots data csv file');
            if (is.null(tbl)) return(NULL);
            if (is.null(out.csv)) out.csv<-file.path(dirname(tbl),'effortByStatArea.csv');
        }
        tbl<-read.csv(tbl,stringsAsFactors=FALSE);
        cat("Done reading input csv file.\n")
    }
    names(tbl)<-tolower(names(tbl));
    #cat("names:\n",paste(names(tbl),collapse=', '),'\n')
    keep<-c('fisheryyear','fisherytype','region','statarea','adfg','trip','sampdate','spn');
    #cat("keep:\n",paste(keep,collapse=', '),'\n')
    tbl<-tbl[,keep];
    
    #recode region 'UNKN' to 'XUNK' to facilitate pivot tables
    idx<-tbl$region=='UNKN';
    tbl$region[idx]<-'XUNK';
    
    #add month and quarter dataframe
    tbl[['month']]<-wtsUtilities::parseMonths(tbl$sampdate,format='MM/DD/YY')
    tbl[['qrtr']]<-calcCrabFisheryQuarterFromMonth(tbl$month);
    
    #add unique ids for trips and sampled pots
    tbl$trip<-paste(tbl$adfg,tbl$trip,sep='_');
    tbl$spn<-paste(tbl$trip,tbl$spn,sep='_');
        
    #extract summary data of interest
    qry<-"select
          fisheryyear,
          qrtr as fisheryquarter,
          fisherytype,
          region,
          statarea,
          count(distinct adfg) as vessels,
          count(distinct trip) as trips,
          count(distinct spn) as pots
        from 
          tbl
        group by
          fisheryyear,fisheryquarter,fisherytype,region,statarea
        order by
          fisheryyear,fisheryquarter,fisherytype,region,statarea;";
    tbleff<-sqldf(qry);
  
  if (!is.null(out.csv)&!(is.na(out.csv))) {
      write.csv(tbleff,file=out.csv,row.names=FALSE);
  }
  
  return(tbleff);
}

#dfr.eff.summary<-calcEffort.ByStatArea()
