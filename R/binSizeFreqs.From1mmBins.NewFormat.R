#'
#'@title Re-bin and export size frequencies based on a csv file with annual (new format) ADF&G 1-mm size bin counts.
#'
#'@description Function to re-bin and export size frequencies based on annual ADF&G 1-mm size bin counts as csv files.
#'
#'@details Input dataframe/csv should have columns year,region,sex,shellcondition,size,frequency
#'
#'@param dfr - dataframe with raw size freq.s (or csv filename to read, or NULL for prompt)
#'@param aggByRegion - flag (T/F) to aggregate size comps by region
#'@param aggBySex - flag (T/F) to aggregate size comps by sex
#'@param aggByShellCondition  - flag (T/F) to aggregate size comps by shell condition
#'@param cutpts - vector of cut points for re-binned size freq.s
#'@param truncate.low  - flag (T/F) to exclude sizes smaller than the 1st cutpoint
#'@param truncate.high - flag (T/F) to exclude sizes larger than the last cutpoint
#'
#'@return A dataframe containing the re-binned size frequencies.
#'
#'@import sqldf
#'@importFrom wtsUtilities selectFile
#'
#'@export
#'
#library('sqldf');
binSizeFreqs.From1mmBins.NewFormat<-function(dfr=NULL,
                                             aggByRegion=TRUE,
                                             aggBySex=FALSE,
                                             aggByShellCondition=FALSE,
                                             cutpts=seq(from=25,to=185,by=5),
                                             truncate.low=TRUE,
                                             truncate.high=FALSE,
                                             save=TRUE,
                                             out.dir=NULL,
                                             out.csv=NULL){
    if (!is.data.frame(dfr)){
        if (is.null(dfr)){
            #get csv file
            dfr<-wtsUtilities::selectFile(ext='csv',caption="Select new-format ADF&G observer data file with annual counts in 1-mm sizebins");
        }
        #get output directory and read dataframe
        if (is.null(out.csv)) {
            out.csv<-dfr;#take as basename
            if (is.null(out.dir)) out.dir<-dirname(out.csv);
            out.csv<-basename(out.csv);
            out.csv<-paste('rebinned.',substr(out.csv,1,nchar(out.csv)-4),sep='');#basename w/out extension
        }
        dfr<-read.csv(dfr,stringsAsFactors=FALSE);
    }
    names(dfr)<-tolower(names(dfr));
    
    if (is.null(out.dir)) out.dir<-getwd();#default to working directory for output files

    #define cutpoints
    minZ<-min(cutpts,na.rm=TRUE);
    maxZ<-max(cutpts,na.rm=TRUE);
    ctpts.tmp<-cutpts
    if (!truncate.low ) {ctpts.tmp[1]<-0;}
    if (!truncate.high) {ctpts.tmp[length(ctpts.tmp)]<-Inf;}
    
    cuts<-cut(dfr$size,ctpts.tmp,right=FALSE,labels=FALSE)
    dfr$bin<-cutpts[cuts];
    
    #convert bins to a dataframe for sqldf
    bins<-as.data.frame(cutpts[1:(length(cutpts)-1)])
    names(bins)<-"bin";
    
    #define factors
    facs<-'year';
    if (!aggByShellCondition){facs<-paste('shellcondition',facs,sep=',');}
    if (!aggBySex)           {facs<-paste('sex',           facs,sep=',');}
    if (!aggByRegion)        {facs<-paste('region',        facs,sep=',');}
    
    qry<-"select
             &&facs,
             bin,
             sum(frequency) as frequency
          from
              dfr
          group by
              &&facs,bin
          order by
              &&facs,bin;";
    qry<-gsub("&&facs",facs,qry);
    dfr1<-sqldf(qry);
    
    #get unique factor values
    qry<-"select distinct
             &&facs
          from
              dfr
          order by
              &&facs;";
    qry<-gsub("&&facs",facs,qry);
    uniqFs<-sqldf(qry);
    
    #make table of unique factor levels and all size bins
    qry<-"select &&facs, bin
          from
              uniqFs, bins
          order by
              &&facs, bin;";
    qry<-gsub("&&facs",facs,qry);
    uniqFZs<-sqldf(qry);
    
    #expand binned size frequencies to unique factor levels, size bins
    uFZs<-paste("u.",names(uniqFZs),sep='')
    dFZs<-paste("d.",names(uniqFZs),sep='')
    jc<-paste(dFZs,uFZs,sep='=',collapse=' and\n')
    uFZs<-paste(uFZs,collapse=',');
    qry<-"select
            &&uFZs,
            d.frequency as frequency
          from
            uniqFZs u left join
            dfr1 d
          on
            &&jc
          order by
            &&uFZs;";
    qry<-gsub("&&uFZs",uFZs,qry);
    qry<-gsub("&&jc",jc,qry);
    cat(qry,'\n');
    dfr2<-sqldf(qry);
    dfr2$frequency[is.na(dfr2$frequency)]<-0;
    
    if (save){
        if (is.null(out.dir)) out.dir<-getwd();
        if (is.null(out.csv)) out.csv<-"SizeFreqs";
        
        csv<-file.path(out.dir,paste(out.csv,'.csv',sep=''));
        write.csv(dfr2,file=csv,row.names=FALSE);
    }
    
    return(invisible(dfr2));
    
#     #get sample sizes (numbers of individuals)
#     qry<-"select 
#             &&facs,
#             sum(frequency) as ss
#           from
#             dfr2
#           group by
#             &&facs
#           order by
#             &&facs;";
#     qry<-gsub("&&facs",facs,qry);
#     ss<-sqldf(qry);
#     
#     #haven't thought this bit out yet for exporting
#     #wide-format size freq's by factor levels
#     if (ncol(uniqFs)>1){
#         dfrFLBs<-uniqFs[,-ncol(uniqFs)];#facs less bins
#         qry<-"select distinct *
#               from dfrFLBs;";
#         uniqFLBs<-sqldf(qry);
#         for (iFLB in 1:nrow(uniqFLBs)){
#             uFLB<-uniqFLBs[iFLB,];
#             qry<-"select
#                     &&cols,
#                     frequency
#                   from 
#                     dfr2 as d,
#                     uFLB as u
#                   where
#                     wc
#                  order by
#                     &&cols;"
#         }
#     }
#     
#     #loop over cols, create size frequencies by year for each col, export to csv in 'wide' format
#     res<-list();
#     for (col in cols){
#         if (any(col==names(dfr))){
#             qry<-gsub("sex_sc",col,qryp);
#             dfrp<-sqldf(qry);
#             dfrp$tot[is.na(dfrp$tot)]<-0;
#             #reshape table to wide format
#             wd.dfrp<-reshape(dfrp,direction='wide',
#                           idvar=c('year'),
#                           v.names='tot',
#                           timevar='bin');
#             nmcols<-names(wd.dfrp);
#             nmcols<-gsub("tot.","",nmcols);
#             names(wd.dfrp)<-nmcols;
#             out.csv<-file.path(dirnm,paste(basenm,col,"csv",sep="."))
#             write.csv(wd.dfrp,out.csv,row.names=FALSE);
#             res[[col]]<-dfrp;
#         }
#     }
#     return(invisible(res));
}

#dfr<-binSizeFreqs.From1mmBins.NewFormat(save=FALSE);
