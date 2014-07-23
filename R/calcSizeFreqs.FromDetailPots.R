#'
#'@title Calculate size frequencies from ADF&G detailed "measure pot" data.
#'
#'@description Calculate size frequencies from ADF&G detailed "measure pot" data.
#'
#'@details Calculate size frequencies from ADF&G detailed "measure pot" data.
#'
#'@param tbl - dataframe derived from old-format ADF&G observed size frequencies csv file, or csv file itself
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
calcSizeFreqs.FromDetailPots<-function(tbl=NULL,
                                        cutpts=seq(from=25,to=185,by=5),
                                        truncate.low=FALSE,
                                        truncate.high=FALSE,
                                        csv.base=NULL){
  
  #create detailed 'measure' pots data table (if not an input)
  if (!is.data.frame(tbl)){
    cat("Reading ADF&G detailed 'measure' pots csv file.\n")
    if (is.null(tbl)) {
        tbl<-wtsUtilities::getCSV(caption='Select ADF&G detailed "measure" pots data csv file');
        if (is.null(tbl)) return(NULL);
    } else {
        tbl<-read.csv(tbl,stringsAsFactors=FALSE);
    }
    cat("Done reading input csv file.\n")
  }
  names(tbl)<-tolower(names(tbl));
  
  qry<-"select
          sex as sex,
          shell as shell,
          size as size,
          count(size) as freq
        from 
          tbl
        group by
          sex, shell, size
        order by
          sex, shell, size;"
  tbl1<-sqldf(qry);

  uniqX<-as.data.frame(list(sex =c("FEMALE","MALE"),
                            code=c(2,1)));
    
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
# 
# csv.in<-"SnowCrabFishery2012-13_TannerAllCrab.csv"
# res<-calcSizeFreqs.FromMeasurePots(tbl=csv.in,
#                                    cutpts=seq(from=25,to=185,by=5),
#                                    truncate.low=FALSE,
#                                    truncate.high=FALSE,
#                                    csv.base="SnowCrabFishery2012-13_TannerAllCrabBinnedSizeFreqs.csv")
