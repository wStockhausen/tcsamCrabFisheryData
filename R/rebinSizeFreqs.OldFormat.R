#'
#'@title Re-bin size frequencies from annual ADF&G observed size frequencies (in old format csv files).
#'
#'@description Function to re-bin and export size frequencies based on annual ADF&G 1-mm size bin counts as csv files.
#'
#'@details This function takes a dataframe derived from an old-format ADF&G observed size frequencies
#'file, bins it, and converts it to wide- and long-format size frequency dataframes, as well as an
#'associated table of sample sizes. In the old format, each file represents total numbers of 
#'at-sea observer-measured crabs for a single year, classified by shell condition categories 
#''newsoft', 'pliable', 'new', 'old' and 'very old' (each a column), 
#'as well as by 'sex' and 'size' (columns).
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
#library(sqldf);
rebinSizeFreqs.OldFormat<-function(tbl=NULL,
                                   cutpts=seq(from=25,to=185,by=5),
                                   truncate.low=TRUE,
                                   truncate.high=FALSE,
                                   csv.base=NULL){
  
  #
  if (!is.data.frame(tbl)){
    cat("Reading ADF&G size frequency csv file.\n")
    if (is.null(tbl)) {
        tbl<-wtsUtilities::getCSV(caption='Select old-format ADF&G at-sea observer size frequency csv file');
        if (is.null(tbl)) return(NULL);
    } else {
        tbl<-read.csv(tbl,stringsAsFactors=FALSE);
    }
    cat("Done reading input csv file.\n")
  }
  names(tbl)<-tolower(names(tbl));
  
  nCtPts<-length(cutpts)
  ctpts.tmp<-cutpts
  if (!truncate.low ) ctpts.tmp[1]<-0;
  if (!truncate.high) ctpts.tmp[nCtPts]<-Inf;
  
  cuts<-cut(tbl$size,ctpts.tmp,right=FALSE,labels=FALSE)
  tbl$size<-cutpts[cuts];
  
  tblCutpts<-as.data.frame(list(size=cutpts[1:(nCtPts-1)]));#lower cutpts
  tblSexs<-as.data.frame(list(sex=c("FEMALE","MALE")))
  qry<-"select
          sex as sex,
          size as size
        from
          tblSexs,
          tblCutpts";
  tblUniqXZs<-sqldf(qry);
  
  qry<-"select
          sex as sex,
          size as size,
          newsoft+pliable+new as new_shell,
          old+veryold as old_shell
        from
          tbl;"
  tbl1<-sqldf(qry);
  
  qry<-"select
          sex as sex,
          size as size,
          sum(new_shell) as new_shell,
          sum(old_shell) as old_shell,
          sum(new_shell+old_shell) as all_shell
        from
          tbl1
        group by
          sex, size
        order by
          sex, size;"
  tbl2<-sqldf(qry);
  
  qry<-"select
          xz.sex as sex,
          xz.size as size,
          t.new_shell as new_shell,
          t.old_shell as old_shell,
          t.all_shell as all_shell
        from
          tblUniqXZs as xz left join
          tbl2 as t
        on
          xz.sex  = t.sex  and
          xz.size = t.size
        order by
          sex, size;"
  tbl3<-sqldf(qry);
  
  tbl3$new_shell[is.na(tbl3$new_shell)]<-0;
  tbl3$old_shell[is.na(tbl3$old_shell)]<-0;
  tbl3$all_shell[is.na(tbl3$all_shell)]<-0;
  
  #reshape table to long format
  tbl4<-reshape(tbl3,direction='long',
                idvar=c('sex','size'),
                v.names='freq',
                timevar="shell_condition",
                times=c('new_shell','old_shell','all_shell'),
                varying=list(shell=c('new_shell','old_shell','all_shell')));
  
  qry<-"select
          sex as sex,
          shell_condition as shell_condition,
          size as size,
          freq as freq
        from
          tbl4
        order by
          sex,shell_condition,size;"
  tbl5<-sqldf(qry);
  
  qry<-"select
          sex as sex,
          shell_condition as shell_condition,
          sum(freq) as sample_size
        from
          tbl5
        group by
          sex, shell_condition
        order by
          sex, shell_condition;"
  tbl.ss<-sqldf(qry);
  
  #reshape table to wide format
  tbl.wd<-reshape(tbl5,direction='wide',
                  idvar=c('sex','shell_condition'),
                  v.names='freq',
                  timevar='size');
  
  if (!is.null(csv.base)) {
    write.csv(tbl5,  file=paste(csv.base,".long.csv",sep=''),row.names=FALSE);
    write.csv(tbl.wd,file=paste(csv.base,".wide.csv",sep=''),row.names=FALSE);
    write.csv(tbl.ss,file=paste(csv.base,".ss.csv",sep=''),  row.names=FALSE);
  }
  
  return(list(long=tbl3,wide=tbl.wd,sample.size=tbl.ss))
}

# csv.in<-"SnowCrabFishery2012-13_TannerSizeFreqsBySex.csv"
# SizeFreqs.SCF<-binSizeFreqs.OldFormat(csv.in=csv.in,
#                                       cutpts=seq(from=25,to=185,by=5),
#                                       csv.base="TCSAM2012-13_SCF.BinnedSizeFreqs")
# 
# csv.in<-"BBRKCFishery2012-13_TannerSizeFreqsBySex.csv"
# SizeFreqs.RKF<-binSizeFreqs.OldFormat(csv.in=csv.in,
#                                       cutpts=seq(from=25,to=185,by=5),
#                                       csv.base="TCSAM2012-13_RKF.BinnedSizeFreqs")
# 
# csv.in<-"../2009-10/TannerCrabFishery2009-10_TannerCrabRetainedSizeFreqsBySex.csv"
# SizeFreqs.TCFR<-binSizeFreqs.OldFormat(csv.in=csv.in,
#                                        cutpts=seq(from=25,to=185,by=5),
#                                        csv.base="../2009-10/TCSAM2009-10_TCF.RetainedBinnedSizeFreqs")
