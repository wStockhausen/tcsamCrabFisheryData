##########################################################
#Plot dockside size comps for crab for 1 year.           #
##########################################################
source(file.path(Sys.getenv('R_CodeCrab'),"plotStackedBars.dataframe.R"),chdir=TRUE);
source(file.path(Sys.getenv('R_SCRIPTS'),"Utilities/rebinColumn.R"),chdir=TRUE);
source(file.path(Sys.getenv('R_SCRIPTS'),"Utilities/sumBy.R"),chdir=TRUE);
plotDocksideSizeComps<-function(dfr=NULL,
                                fn=NULL,
                                xCol='size',
                                factors=c(),
                                sortBy=NULL,
                                ascending=TRUE,
                                yCols=c('new','old'),
                                yLabs=c("new shell",'old shell'),
                                binwidth=5,
                                cutpts=seq(from=0,to=200,by=binwidth),
                                barwidth=100,
                                main='landed catch',
                                vertLines=c(120),
                                xlims=c(0,200),
                                ylims=NULL,
                                xlab="size (cm CW)",
                                ylab="count",
                                units="data units",
                                xaxt=TRUE,
                                yaxt=TRUE,
                                debug=FALSE){
  
  #if necessary, read data file
  if (is.null(dfr)){
    if (!is.null(fn)){
      dfr<-read.csv(fn,stringsAsFactors=FALSE)
      names(dfr)<-tolower(names(dfr));
    }
  }
  
  #rebin dataframe using cutpts
  res<-rebinColumn(dfr,column=xCol,cutpts=cutpts,binType='C');
  #recalc frequencies based on new bins
  dfrp<-sumBy(res$tbl,c(xCol,factors),vars=yCols);
  
  nf<-length(factors);
  if (nf==0){
    #plot stacked bars
    plotStackedBars.dataframe(dfrp,
                              xCol=xCol,
                              yCols=yCols,
                              yLabs=yLabs,
                              binType='C',
                              barwidth=barwidth,
                              vertLines=vertLines,
                              main=main,
                              xlims=xlims,
                              ylims=ylims,
                              xlab=xlab,
                              ylab=ylab);    
  } else {
    #get factor levels
    us<-getFactorLevels(dfrp,factors,sortBy=sortBy,ascending=ascending);
    #select rows from dfr corresponding to the selected factor levels
    nu<-row(us);
    for (iu in 1:nu){
      u<-us[iu,];
      idx<-dfrp[[factors[1]]]==u[1,factors[1]];
      txt<-paste(factors[1],"=",u[1,factors[1]]);
      if (nf>1){
        for (i in 2:nf){
          idx<-idx & dfrp[[factors[i]]]==u[1,factors[i]];
          txt<-paste(txt,", ",factors[i],"=",u[1,factors[i]]);
        }
      }
      dfrpp<-dfrp[idx,];
      #plot stacked bars
      plotStackedBars.dataframe(dfrpp,
                                xCol=xCol,
                                yCols=yCols,
                                yLabs=yLabs,
                                binType='C',
                                barwidth=barwidth,
                                vertLines=vertLines,
                                main=main,
                                subtitle=txt,
                                xlims=xlims,
                                ylims=ylims,
                                xlab=xlab,
                                ylab=ylab);
    }
  }
  
  return(dfrp);
}

fn<-'tanner09docksideSF.csv';
dfr<-plotDocksideSizeComps(fn=fn);
