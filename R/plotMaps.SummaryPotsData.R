#'
#'@title Plot aggregated data from ADF&G "all pots" (SummaryPots) data files.
#'
#'@description Function that will plot aggregated data from ADF&G "all pots" (SummaryPots) data files.
#'
#'@details Plots maps.
#'
#'@param tbl - dataframe with aggregated data derived from "all pots" (SummaryPots) csv file, or csv file itself
#'@param plotTypes - vector of data types to plot {'vessels','pots','female','sublegal','legal'}
#'@param zscls - NULL or list w/ plot types as names. Associated values will be used as z-scales.
#'@param byQuarter - flag to plot maps by fishery quarter
#'@param byFishery - flag to plot maps by fishery type
#'@param cleanup - flag to delete temporary files
#'
#'@return list with z-scales for each plot type.
#'
#'@import sqldf
#'@importFrom wtsUtilities getCSV
#'@importFrom wtsGMT plotMap.CSV
#'@importFrom wtsGMT createPDF.fromPS
#'
#'@export
#' 
plotMaps.SummaryPotsData<-function(tbl=NULL,
                                   plotTypes=c('vessels','pots','female','sublegal','legal'),
                                   zscls=NULL,
                                   byQuarter=FALSE,
                                   byFishery=FALSE,
                                   cleanup=TRUE){
    if (is.null(zscls)){
        zscls<-vector(mode='list',length=length(plotTypes));
        names(zscls)<-plotTypes;
    }
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
        psFiles<-vector(mode='character',length=0);
        zscl<-NA;
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
                        zsclp<-wtsGMT::plotMap.CSV(dfr=tblp,lat='lat',lon='lon',
                                                    title="",year=yrstr,
                                                    col=col,zunits=zunits,zlab=zlab,zscl=zscls[[plotType]],
                                                    delx=1,dely=0.5,rotate=TRUE,elev=70,
                                                    blocktype='SUM',plt_blocktype='COARSE',plt_blocklocations=FALSE,
                                                    plt_surface=TRUE,plt_stations=TRUE,
                                                    plt_reflines=TRUE,
                                                    psFile=psFile,
                                                    cleanup=cleanup);
                        zscl<-max(zscl,zsclp,na.rm=TRUE)
                        psFiles<-c(psFiles,paste(psFile,'.ps',sep=''));
                    }
                }#f
            }#q
        }#y
        if (is.null(zscls[[plotType]])) {zscls[[plotType]]<-zscl;}
        wtsGMT::createPDF.fromPS(base.ps,psFiles=psFiles)
        if (cleanup){file.remove(psFiles);}
    }#plotType
    
    cat("Z-scales used to plot maps were: \n")
    print(zscls);
    
    return(zscls);
}

#plotMaps.SummaryPotsData(tbl2[tbl2$fisheryyear==2011,],plotTypes='pots',cleanup=FALSE)
#zscls<-plotMaps.SummaryPotsData(tbl2)
#zsclsp<-plotMaps.SummaryPotsData(tbl2,zscls=zscls)

