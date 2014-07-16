#'
#'@title Plot aggregated data from ADF&G "all pots" (ObservedSummaryPots) data files.
#'
#'@description
#'
#'@details 
#'
#'@param tbl - dataframe with aggregated data derived from "all pots" (ObservedSummaryPots) csv file, or csv file itself
#'@param byQuarter - flag to plot maps by fishery quarter
#'@param byFishery - flag to plot maps by fishery type
#'
#'@return list with 
#'
#'@import sqldf
#'@importFrom wtsUtilties getCSV
#'@importFrom wtsGMT plotMap.CSV
#'@importFrom wtsGMT createPDF.fromPS
#'
#'@export
#' 
plotMaps.ObservedSummaryPots<-function(tbl=NULL,
                                       plotTypes=c('vessels','pots','female','sublegal','legal'),
                                       byQuarter=FALSE,
                                       byFishery=FALSE,
                                       cleanup=TRUE){
    
    qry<-"select distinct
        fisheryyear as year
    from tbl
    order by year;";
    uniqYs<-sqldf(qry);
    cat("uniqYs:",'\n')
    print(uniqYs);
    
    qry<-"select distinct
        fisherytype
    from tbl
    order by fisherytype;";
    uniqFs<-sqldf(qry);
    cat("uniqFs:",'\n')
    print(uniqFs);
    
    qry<-"select distinct
        fisheryquarter as quarter
    from tbl
    order by quarter;";
    uniqQs<-sqldf(qry);
    cat("uniqQs:",'\n')
    print(uniqQs);
        
    for (plotType in plotTypes){
        col<-plotType;
        zunits<-plotType;
        zlab<-plotType;
        if      (plotType=='pots')     {base.ps<-'mapSamplePots';}
        else if (plotType=='vessels')  {base.ps<-'mapObservedVessels';}
        else if (plotType=='female')   {base.ps<-'mapFemales';               zunits<-'crab';zlab<-'females';}
        else if (plotType=='sublegal') {base.ps<-'mapMales.Sublegal';        zunits<-'crab';zlab<-'sublegals';}
        else if (plotType=='legalnr')  {base.ps<-'mapMales.LegalNotRetained';zunits<-'crab';zlab<-'discarded legals';}
        else if (plotType=='legalret') {base.ps<-'mapMales.LegalRetained';   zunits<-'crab';zlab<-'retained legals';}
        else if (plotType=='legal')    {base.ps<-'mapMales.AllLegal';        zunits<-'crab';zlab<-'all legals;'}
        psFiles<-vector(mode='character',length=0)
        for (y in uniqYs[["year"]]){
            for (q in uniqQs[["quarter"]]){
                for (f in uniqFs[["fisherytype"]]){
                    yrstr<-as.character(y);
                    if (byQuarter) {yrstr<-paste(yrstr,".Q",q,sep='');}
                    if (byFishery) {yrstr<-paste(yrstr,".",f,sep='');}
                    cat("Plotting map for ",yrstr,'\n')
                    tblp<-tbl[(tbl$fisheryyear==y)&(tbl$fisherytype==f)&(tbl$fisheryquarter==q),];
                    print(tblp);
                    if (nrow(tblp)>0){
                        psFile<-paste(base.ps,yrstr,sep='');
                        wtsGMT::plotMap.CSV(dfr=tblp,lat='lat',lon='lon',
                                            title="",year=yrstr,
                                            col=col,zunits=zunits,zlab=zlab,
                                            delx=1,dely=0.5,rotate=TRUE,elev=70,
                                            blocktype='SUM',plt_blocktype='COARSE',plt_blocklocations=FALSE,
                                            plt_surface=TRUE,plt_stations=TRUE,
                                            plt_reflines=TRUE,
                                            psFile=psFile,
                                            cleanup=cleanup);
                        psFiles<-c(psFiles,paste(psFile,'.ps',sep=''));
                    }
                }#f
            }#q
        }#y
        wtsGMT::createPDF.fromPS(base.ps,psFiles=psFiles)
        if (cleanup){file.remove(psFiles);}
    }#plotType
}

#plotMaps.ObservedSummaryPots(tbl2[tbl2$fisheryyear==2011,],plotTypes='pots',cleanup=FALSE)
#plotMaps.ObservedSummaryPots(tbl2)
