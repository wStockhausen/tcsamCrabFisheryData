#'
#'@title Calculate size frequencies by stat area from ADF&G detailed "measure" pot data.
#'
#'@description Function to calculate size frequencies by stat area 
#'from ADF&G detailed "measure" pot data.
#'
#'@details Function to calculate size frequencies by stat area 
#'from ADF&G detailed "measure" pot data.
#'
#'@param tbl - dataframe derived from ADF&G detailed ('measure') pot data.
#'@param cutpts - vector of cut points for binning
#'@param truncate.low  - flag (T/F) to exclude sizes smaller than the 1st cutpoint
#'@param truncate.high - flag (T/F) to exclude sizes larger than the last cutpoint
#'@param csv.base - filename base for output csv files 
#'
#'@return list with 3 elements
#'* long - size frequencies in long format
#'* wide - size frquencies in wide format
#'* sample.size - associated sample sizes
#'
#'@import sqldf
#'@importFrom wtsUtilities getCSV
#'
#'@export
#' 
calcSizeFreqs.FromDetailPots.BtStatArea<-function(tbl=NULL,
                                                  cutpts=seq(from=25,to=185,by=5),
                                                  truncate.low=FALSE,
                                                  truncate.high=FALSE,
                                                  csv.base=NULL){
  
    #create detailed 'measure' pots data table (if not an input)
    if (!is.data.frame(tbl)){
        cat("Read ADF&G detailed 'measure' pots csv file.\n")
        if (is.null(tbl)) {
            tbl<-wtsUtilities::getCSV(caption='Select ADF&G detailed "measure" pots data csv file');
            if (is.null(tbl)) return(NULL);
        } else {
            tbl<-read.csv(tbl,stringsAsFactors=FALSE);
        }
        cat("Done reading input csv file.\n")
    }
    names(tbl)<-tolower(names(tbl));
    keep<-c('fisheryyear','fisherytype','adfg','sampdate','spn','statarea',
            'sex','size','legal','shell');
    tbl<-tbl[,keep];
    
    #add month and quarter dataframe
    tbl[['month']]<-wtsUtilities::parseMonths(tbl$sampdate,format='MM/DD/YY')
    tbl[['qrtr']]<-getCrabFisheryQuarterFromMonth(tbl$month);
    
    #add unique ids for trips and sampled pots
    tbl$trip<-paste(tbl$adfg,tbl$trip,sep='_');
    tbl$spn<-paste(tbl$trip,tbl$spn,sep='_');
    
    #extract summary data of interest
    qry<-"select
          fisheryyear,
          qrtr as fisheryquarter,
          fisherytype,
          statarea,
          count(distinct adfg) as vessels,
          count(distinct trip) as trips,
          count(distinct spn) as pots
        from 
          tbl
        group by
          fisheryyear,fisheryquarter,fisherytype,statarea
        order by
          fisheryyear,fisheryquarter,fisherytype,statarea;";
    tblsd<-sqldf(qry);

    #define cutpoints
    minZ<-min(cutpts,na.rm=TRUE);
    maxZ<-max(cutpts,na.rm=TRUE);
    ctpts.tmp<-cutpts
    if (!truncate.low ) {minZ<-0;   ctpts.tmp[1]<-minZ;}
    if (!truncate.high) {maxZ<-Inf; ctpts.tmp[length(ctpts.tmp)]<-maxZ;}
    
    cuts<-cut(tbl$size,ctpts.tmp,right=FALSE,labels=FALSE)
    tbl$cuts<-cutpts[cuts];

    #define some auxilliary tables
    cnvSXs<-as.data.frame(list(val =c("FEMALE","MALE","UNKNOWN"),
                               code=c(2,1,0)));
    cnvSCs<-as.data.frame(list(val =c("NEW","NEW","NEW","OLD","OLD"),
                               code=c(1,9,2,3,4)));
    cnvLGs<-as.data.frame(list(val =c("ILLEGAL","ILLEGAL","LEGAL","LEGAL"),
                               code=c(-7,0,1,2)));
  
    #extract data of interest
    qry<-"select
          fisheryyear,
          qrtr as fisheryquarter,
          fisherytype,
          statarea,
          sx.val as SEX,
          sc.val as shellcondition,
          lg.val as LEGAL,
          cuts,
          size,
          count(*) as freq
        from 
          tbl t,
          cnvSXs sx,
          cnvSCs sc,
          cnvLGs lg
        where
          t.sex=sx.code and
          t.shell=sc.code and
          t.legal=lg.code
        group by
          fisheryyear,qrtr,fisherytype,statarea,sx.val,sc.val,lg.val,cuts,size
        order by
          fisheryyear,fisheryquarter,fisherytype,statarea,SEX,LEGAL,cuts,size,shellcondition;"
  tbl1<-sqldf(qry);
  names(tbl1)<-tolower(names(tbl1));
    
  qry<-"select distinct
          shell as sc
        from 
          tbl
        order by
          sc;"
  uniqSC<-sqldf(qry);
  uniqSC<-as.data.frame(list(shell_condition=uniqSC$sc[!is.na(uniqSC$sc)]));
  
  uniqZ<-as.data.frame(list(size=1:200));
  
  qry<-"select
          x.sex as sex,
          x.code as sexcode,
          sc.shell_condition as shell_condition,
          z.size as size
        from
          uniqX as x,
          uniqSC as sc,
          uniqZ as z
        order by
          sex, shell_condition, size;"
  uniqXSZ<-sqldf(qry);
  
  qry<-"select
          u.sex as sex,
          u.shell_condition as shell_condition,
          u.size as size,
          sum(t.freq) as freq
        from 
          uniqXSZ as u left join
          tbl1 as t
        on
          u.sexcode         = t.sex   and
          u.shell_condition = t.shell and
          u.size            = t.size
        group by
          u.sex, u.shell_condition, u.size
        order by
          sex, shell_condition, size;"
  tbl2<-sqldf(qry);
  
  tbl2$freq[is.na(tbl2$freq)]<-0;
  
  ctpts.tmp<-cutpts
  if (!truncate.low ) ctpts.tmp[1]<-0;
  if (!truncate.high) ctpts.tmp[length(ctpts.tmp)]<-Inf;
  
  cuts<-cut(tbl2$size,ctpts.tmp,right=FALSE,labels=FALSE)
  tbl2$cuts<-cutpts[cuts];
  
  qry<-"select
          sex as sex,
          shell_condition as shell_condition,
          cuts as size,
          sum(freq) as freq
        from
          tbl2
        group by 
          sex, shell_condition, cuts
        order by 
          sex, shell_condition, size;"
  tbl3<-sqldf(qry);#table in long format
  
  #reshape table to wide format
  tbl4<-reshape(tbl3,direction='wide',
                idvar=c('sex','shell_condition'),
                v.names='freq',
                timevar='size');
  
  if (!is.null(csv.base)) {
      write.csv(tbl3,file=paste("long",csv.base,"csv",sep='.'),row.names=FALSE);
      write.csv(tbl4,file=paste("wide",csv.base,"csv",sep='.'),row.names=FALSE);
  }
  
  return(list(by1mm=tbl2,long=tbl3,wide=tbl4));
}

# csv.in<-"SnowCrabFishery2012-13_TannerAllCrab.csv"
# res<-calcSizeFreqs.FromDetailPots.ByStatArea(tbl=csv.in,
#                                    cutpts=seq(from=25,to=185,by=5),
#                                    truncate.low=FALSE,
#                                    truncate.high=FALSE,
#                                    csv.base="SnowCrabFishery2012-13_TannerAllCrabBinnedSizeFreqs.csv")
