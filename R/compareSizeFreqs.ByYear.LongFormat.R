#'
#'@title Plot multiple size frequencies for comparison by year.
#'
#'@description Function to plot size frequencies by year for visual comparison.
#'
#'@param dfrs - list of dataframes w/ size frequencies to compare
#'@param n - number of size frequency "types" to compare (ignored if dfrs is given)
#'@param lbls - vector of labels for size frequency "types" to use in plot legend
#'@param yCol - dataframe column name with year info
#'@param zCol - dataframe column name with size info
#'@param fCol - dataframe column name with frequency info
#'@param norm - flag (T/F) to plot normalized size frequencies
#'@param alternate - flag (T/F) to alternate positive/negative scales for frequencies
#'@param ylim - range of years to plot (can be NULL)
#'@param zlim - range of sizes to plot (can be NULL)
#'@param fmax - max frequency scale for plot (can be NULL)
#'@param samePlotScale - flag (T/F) to use same frequency scale for all plots
#'@param vlines - sizes at which to plot vertical reference lines
#'@param ltys - vector of line types to use to plot size freq.s
#'@param cols - vector of colors to use to plot size freq.s
#'@param alpha - transparency value (0-1) to use for areas
#'@param plotPDF - flag (T/F) to output plots to a pdf file
#'@param nc - number of columns per page in which to arrange plots for pdf output
#'@param nr - number of rows per page in which to arrage plots for pdf output 
#'@param pdfFile - name of output pdf file
#'
#'@details 
#'Dataframes should be derived from "long" format csv files for annual size frequencies.
#'
#'If 'dfrs' is NULL, then 'n' represents the number of size frequency files the user will
#'be prompted to specify for the comparison. 
#'
#'@import wtsUtilities
#'
#'@export
#'
compareSizeFreqs.ByYear.LongFormat<-function(dfrs=NULL,
                                              n=1,
                                              lbls=NULL,
                                              yCol='year',
                                              zCol='size',
                                              fCol='frequency',
                                              norm=TRUE,
                                              alternate=FALSE,
                                              ylim=NULL,
                                              zlim=NULL,
                                              fmax=NULL,
                                              samePlotScale=TRUE,
                                              vlines=c(127,140),
                                              ltys=1:4,
                                              cols=c("blue","green","cyan","gray"),
                                              alpha=0.5,
                                              plotPDF=FALSE,
                                              nc=4,
                                              nr=5,
                                              pdfFile="compZFs.pdf"){
    
    if (is.null(dfrs)){
        flg<-FALSE;
    } else {
        flg<-TRUE;
        n<-length(dfrs);
    }
    
    ltys<-rep(ltys,length.out=n);
    cols<-rep(cols,length.out=n);
    
    ylimp<- NA;
    zlimp<- NA;
    fmxp<- NA;
    dfrsp<-vector(mode='list',length=n)
    for (i in 1:n) {
        if (flg){
            dfr<-dfrs[[i]];
        } else {
            dfr<-wtsUtilities::getCSV(caption=paste("Select ",i,"th size frequency csv file",sep=""))
            if (is.null(dfr)) {return(NULL);} #user cancelled
        }
        yrs<-sort(unique(dfr[[yCol]]));
        ylimp<-range(ylimp,yrs,na.rm=TRUE);
        zlimp<-range(zlimp,dfr[[zCol]],na.rm=TRUE);
        for (y in yrs){
            frq<-dfr[dfr[[yCol]]==y,fCol];
            if (norm) {
                fmxp<-max(fmxp,frq/sum(frq),na.rm=TRUE);
            } else {
                fmxp<-max(fmxp,frq,na.rm=TRUE);
        }
        }
        dfrsp[[i]]<-list(yrs=yrs,dfr=dfr);
    }
    
    #set global scales
    if (is.null(ylim)) ylim<-ylimp;
    if (is.null(zlim)) zlim<-zlimp;
    if (is.null(fmax)) fmax<-fmxp;
    
    if (plotPDF){
        par.old<-par(mfrow=c(nr,nc),mai=c(0.0, 0.0, 0.0, 0.0),cex=.5,omi=c(0.5,0.5,0.5,0.5))
        on.exit(par(par.old));
        dev<-pdf(file=pdfFile,8.5,11);
    }
    
    ylab<-"frequency";
    if (norm) {ylab<-"normalized frequency";}
    yv<-c(0,1); 
    if (alternate) yv<-c(-1,1);
    normf<-1;
    for (y in ymn:ymx){
        #determine f-scale for this plot
        fmxp<-NA;
        for (i in 1:n){
            frq<-as.vector(t(dfrsp[[i]]$dfr[dfr[[yCol]]==y,fCol]));
            if (norm) {frq<-frq/sum(frq);}
            fmxp<-max(fmxp,frq,na.rm=TRUE);
        }
        if (!samePlotScale) fmax<-fmxp;
        if (is.finite(fmxp)){
            plot(zlim,fmax*yv,type='n',
                 xlim=zlim,   xlab='size bin (mm)',
                 ylim=1.05*fmax*yv,ylab=ylab);
            rect(zlim[1],1.05*fmax*yv[1],zlim[2],1.05*fmax*yv[2],col="light gray",border=NA)
            for (i in 1:n){
                bns<-as.vector(t(dfrsp[[i]]$dfr[dfr[[yCol]]==y,zCol]));
                frq<-as.vector(t(dfrsp[[i]]$dfr[dfr[[yCol]]==y,fCol]));
                if (norm) {normf<-sum(frq);}
                frq<-frq/normf;
                if (alternate) {frq<-(-1)^(i-1)*frq;}
                nb<-length(bns);
                for (ib in 1:(nb-1)){
                    if (frq[ib]!=0){
                        rect(bns[ib],0,bns[ib+1],frq[ib],border=NA,
                            col=wtsUtilities::addTransparency(cols[i],alpha=alpha));
                    }
                }
                if (frq[nb]!=0){
                    rect(bns[nb],0,bns[nb]+(bns[nb]-bns[nb-1]),frq[nb],border=NA,
                        col=wtsUtilities::addTransparency(cols[i],alpha=alpha));
                }
                lines(bns,frq,lty=ltys[i],col=cols[i],lwd=3,type="s");                
            }
            if (!is.null(vlines)){
                abline(v=vlines,lty=2,col=gray(0.2),lwd=3)
            }
            mtext(paste("year =",y),side=3,adj=0.05)
            if (!is.null(lbls)){
                legend("topleft",lbls,col=cols,lty=ltys)
            }
        }
    }
    if (plotPDF) {dev.off();}
}

#compareSizeFreqs.ByYear.LongFormat(n=2,lbls=c("revised","2013"),plotPDF="TRUE")

# compareSizeFreqs.ByYear.LongFormat(dfrs=list(dfr1,dfr2),
#                                    lbls=c("new shell","old shell"),
#                                    zCol='bin',samePlotScale=TRUE,
#                                    norm=FALSE,alternate=TRUE)
# compareSizeFreqs.ByYear.LongFormat(dfrs=list(dfr1,dfr2),
#                                    lbls=c("new shell","old shell"),
#                                    zCol='bin',samePlotScale=FALSE,
#                                    norm=FALSE,alternate=FALSE)
# compareSizeFreqs.ByYear.LongFormat(dfrs=list(dfr1,dfr2),
#                                    lbls=c("new shell","old shell"),
#                                    zCol='bin',samePlotScale=TRUE,
#                                    norm=TRUE,alternate=TRUE)
