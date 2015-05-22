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
rebinSizeFreqs<-function(dfr=NULL,
                         aggByRegion=TRUE,
                         aggBySex=FALSE,
                         aggByShellCondition=FALSE,
                         cutpts=seq(from=25,to=185,by=5),
                         truncate.low=TRUE,
                         truncate.high=FALSE,
                         save=TRUE,
                         out.dir=NULL,
                         out.csv=NULL);
    mdfr<-NULL;
    if (is.null(dfr)){
        #get csv files
        dfrs<-wtsUtilities::selectFiles(ext='csv',caption="Select ADF&G size frequency files");
        for (dfrn in dfrs){
            dfrp<-rebinSizeBins(dfr=dfrn,
                                aggByRegion=aggByRegion,
                                aggBySex=aggBySex,
                                aggByShellCondition=aggByShellCondition,
                                cutpts=cutpts,
                                truncate.low=truncate.low,
                                truncate.high=truncate.high,
                                save=FALSE,
                                out.dir=NULL,
                                out.csv=NULL);
            mdfr<-rbind(mdfr,dfrp);
        }
        if (save){
            if (is.null(out.dir)) out.dir<-getwd();
            if (is.null(out.csv)) out.csv<-"SizeFreqs.Rebinned";
            
            csv<-file.path(out.dir,paste(out.csv,'.csv',sep=''));
            write.csv(mdfr,file=csv,row.names=FALSE);
        }
        return(invisible)
    }
    if (!is.data.frame(dfr)){
        #get output directory and read dataframe
        if (is.null(out.csv)) {
            out.csv<-dfr;#take as basename
            if (is.null(out.dir)) out.dir<-dirname(out.csv);
            out.csv<-basename(out.csv);
            out.csv<-paste('rebinned.',substr(out.csv,1,nchar(out.csv)-4),sep='');#basename w/out extension
        }
        dfr<-read.csv(dfr,stringsAsFactors=FALSE);
    }
    if (is.null(out.dir)) out.dir<-getwd();#default to working directory for output files
    
    names(dfr)<-tolower(names(dfr));
    
    #new-style datafile
    if (all(names(dfr)==c('year','region','sex','shellcondition','size','frequency'))){
        dfrp<-rebinSizeFreqs.NewFormat(dfr=dfr,
                                   aggByRegion=aggByRegion,
                                   aggBySex=aggBySex,
                                   aggByShellCondition=aggByShellCondition,
                                   cutpts=cutpts,
                                   truncate.low=truncate.low,
                                   truncate.high=truncate.high,
                                   save=save,
                                   out.dir=out.dir,
                                   out.csv=out.csv);
        return(invisible(mdfr))
    }

    if (all(names(dfr)==c('sex','shellcondition','size','frequency'))){
        dfrp<-rebinSizeFreqs.OldFormat(dfr=dfr,
                                       cutpts=cutpts,
                                       truncate.low=truncate.low,
                                       truncate.high=truncate.high,
                                       csv.base=NULL);
        return(invisible(dfrp))
    }
}

