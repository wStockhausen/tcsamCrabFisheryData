#'
#'@title Plot histograms of numbers caught in observed pot hauls by type.
#'
#'@description Function to plot histograms of numbers caught in observed pot hauls by type.
#'
#'@details For observed hauls, plots % of non-zero pot hauls by catch type
#' and histograms of no. caught in pot hauls by catch type.
#' 
#'
#'@param dfr - dataframe (or csv filename) with data from "observed pots"
#'@param vars  - column names with data for plots
#'@param labs  - 'nice' names to associate with column names for plot labels
#'@param maxCatch - value for 'plus' group on number of individuals caught/pot
#'@param noplot - just return the dataframe
#'
#'@import lattice
#'@importFrom wtsUtilities calcPercentNonzero
#'
#'@export
#'
#################################################################
#source(file.path(Sys.getenv('R_SCRIPTS'),"Utilities/calcPercentNonzero.R"),chdir=TRUE);
plotObservedPots.histograms<-function(dfr=NULL,
                                      vars=c('female','sublegal','legalnr','legalret'),
                                      labs=c('female','sublegal males','discarded legal males','retained legal males'),
                                      maxCatch=100,
                                      noplot=FALSE
                                      ){
  
  #if necessary, read data file
    retDFR<-FALSE;
    if (!is.data.frame(dfr)){
        #read in table from csv file
      if (is.null(dfr)) {
        dfr = wtsUtilities::getCSV(caption="Select csv file with crab observer data to plot");
        if (is.null(dfr)) return(NULL);
      } else {
        dfr<-read.csv(dfr,stringsAsFactors=FALSE);
      }
      retDFR<-TRUE;
      names(dfr)<-tolower(names(dfr));
    }
    if (noplot) return(dfr);#simply return the dataframe
  
    #extract data columns and stack
    dfr1<-dfr[,vars];
    names(dfr1)<-labs;
    dfr1<-stack(dfr1,select=labs);
    #apply maxCatch as a plus bin
    dfr1$values<-(dfr1$values<=maxCatch)*dfr1$values+(dfr1$values>maxCatch)*maxCatch;
    
    #compare % non-zero catches among catch types
    dfr.r<-wtsUtilities::calcPercentNonzero(dfr1,vars='values',factors='ind');
    dfr.r$ind<-factor(dfr.r$ind,levels=labs)
  
    bar<-barchart(values~ind,
                  dfr.r,
                  main='Observed Pot Hauls',
                  xlab='catch component',
                  ylab='Fraction Non-zero Pot Hauls');
  
    #compare non-zero catches among catch types
    dfrp<-dfr1[dfr1$values>0,];             #remove zeros
    dfrp$ind<-factor(dfrp$ind,levels=labs); #re-order factors
    
    brks<-c(0,seq(from=5,to=max(dfr1$values),by=5))
    hst<-histogram(~values|ind,
                   dfrp,
                   layout=c(1,length(labs)),
                   breaks=brks,
                   type='count',
                   main='Observed Pot Hauls',
                   xlab='Catch (no. individuals)',
                   ylab='Count of Pots w/ Non-zero Catch');
  
#    plot(bar);
  plot(hst);
  plot(bar,split=c(1,1,1,2));  
  
  return(dfr);
}

#fn<-'tanner09AllObservedPots.csv';
#dfr<-plotObservedPots.histograms(dfr);
