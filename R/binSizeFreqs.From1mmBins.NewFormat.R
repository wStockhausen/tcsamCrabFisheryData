#'
#'@title Re-bin and export annual 'wide'-format, binned size frequencies 
#'based on a csv file with annual (new format) ADF&G 1-mm size bin counts.
#'
#'@description Function to re-bin and export annual 'wide'-format, binned size frequencies 
#'based on annual ADF&G 1-mm size bin counts as csv files.
#'
#'@details Separate csv files will be output for each 'col' in 'cols'.
#'
#'@param cols - names of columns containing frequency data
#'@param bins - vector of size bins
#'
#'@return A list (by data type/original column name) of size frequencies by year as 'long'-format dataframes.
#'The size frequencies are also exported, by data type/original column name, in 'wide' format as
#'csv files in the same folder as the original file.
#'
#'@import sqldf
#'@importFrom wtsUtilities selectFile
#'
#'@export
#'
#library('sqldf');
binSizeFreqs.From1mmBins.NewFormat<-function(cols=c('MALE_NS','MALE_OS','FEMALE_NS','FEMALE_OS'),
                                                    bins=seq(from=25,to=180,by=5)){
    
    #read csv file and create dataframe
    csv<-wtsUtilities::selectFile(ext='csv',caption="Select new-format ADF&G observer data file with annual counts in 1-mm sizebins");
    dfr<-read.csv(csv,stringsAsFactors=FALSE);
    cols<-tolower(cols);
    names(dfr)<-tolower(names(dfr));
    
    #convert bins to a dataframe for sqldf
    bins<-as.data.frame(bins)
    names(bins)<-"bin";
    
    #create table with unique years, size bins
    uniqYrs<-sqldf("select distinct year from dfr order by year;")    
    uniqYZ<-sqldf("select year, bin from uniqYrs, bins order by year, bin;")
    
    qryp<-"select
             u.year,
             u.bin,
             sum(d.sex_sc) as tot
            from
              uniqYZ as u left join
              dfr as d
            on
              u.year=d.year and
              u.bin=d.sizebin
            group by
              u.year, u.bin
            order by
              u.year, u.bin;";
    
    basenm<-basename(csv);                   #get basename
    basenm<-substr(basenm,1,nchar(basenm)-4);#remove extension
    dirnm<-dirname(csv);                     #get path to file folder
    
    #loop over cols, create size frequencies by year for each col, export to csv in 'wide' format
    res<-list();
    for (col in cols){
        if (any(col==names(dfr))){
            qry<-gsub("sex_sc",col,qryp);
            dfrp<-sqldf(qry);
            dfrp$tot[is.na(dfrp$tot)]<-0;
            #reshape table to wide format
            wd.dfrp<-reshape(dfrp,direction='wide',
                          idvar=c('year'),
                          v.names='tot',
                          timevar='bin');
            nmcols<-names(wd.dfrp);
            nmcols<-gsub("tot.","",nmcols);
            names(wd.dfrp)<-nmcols;
            out.csv<-file.path(dirnm,paste(basenm,col,"csv",sep="."))
            write.csv(wd.dfrp,out.csv,row.names=FALSE);
            res[[col]]<-dfrp;
        }
    }
    return(invisible(res));
}

#binSizeFreqs.From1mmBins.NewFormat();
