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
#'@param flim - range for frequency scale on plot (can be NULL)
#'@param samePlotScale - flag (T/F) to use same frequency scale for all plots
#'@param vlines - sizes at which to plot vertical reference lines
#'@param ltys - vector of line types to use to plot size freq.s
#'@param lwd - line width for outlines of size freq.s
#'@param cols - vector of colors to use to plot size freq.s
#'@param alpha - transparency value (0-1) to use for areas
#'@param pdfFile - name of output pdf file (or NULL, if none desired)
#'@param plt.nc - number of columns per page in which to arrange plots for pdf output
#'@param plt.nr - number of rows per page in which to arrage plots for pdf output 
#'
#'@details 
#'Dataframes should be derived from "long" format csv files for annual size frequencies.
#'
#'If 'dfrs' is NULL, then 'n' represents the number of size frequency files the user will
#'be prompted to specify for the comparison. 
#'
#'@import grDevices
#'@importFrom wtsUtilities getCSV
#'@importFrom wtsUtilities addTransparency
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
                                              flim=NULL,
                                              samePlotScale=TRUE,
                                              vlines=c(127,140),
                                              ltys=1,
                                              lwd=2,
                                              cols=c("blue","green","cyan","magenta"),
                                              alpha=0.5,
                                              pdfFile=NULL,
                                              plt.nc=4,
                                              plt.nr=5){
    
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
    flimp<- 0;
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
            if (norm)      {frq<-frq/sum(frq);}
            if (alternate) {frq<-(-1)^(i-1)*frq;}
            flimp<-range(flimp,frq,na.rm=TRUE);
        }#y loop
        dfrsp[[i]]<-list(yrs=yrs,dfr=dfr);
    }#i loop
    remove(yrs,dfr);
    
    #set global scales
    if (is.null(ylim)) ylim<-ylimp;
    if (is.null(zlim)) zlim<-zlimp;
    if (is.null(flim)) flim<-flimp;
    
    #set up pdf device, if required
    plotPDF<-FALSE;
    if (!is.null(pdfFile)){
        plotPDF<-TRUE;
        dev<-pdf(file=pdfFile,8.5,11);
        par.old<-par(mfrow=c(plt.nr,plt.nc),mar=c(3, 2, 0, 1),cex=.5,omi=c(0.5,0.5,0.5,0.5))
        on.exit(par(par.old));
    }
    
    #start plotting
    xlab<-'size bin (mm)';
    ylab<-"frequency";
    if (norm) {ylab<-"normalized frequency";}
    if (plotPDF){
        xlabp<-'';   ylabp<-'';
    } else {
        xlabp<-xlab; ylabp<-yab;
    }
    ctr<-0;
    for (y in ylim[1]:ylim[2]){
        cat("Processing plot for ",y,"\n")
        #determine f-scale for this plot
        flmp<-0;
        for (i in 1:n){
            dfr<-dfrsp[[i]]$dfr;
            frq<-as.vector(t(dfr[dfr[[yCol]]==y,fCol]));
            if (norm)      {frq<-frq/sum(frq);}
            if (alternate) {frq<-(-1)^(i-1)*frq;}
            flmp<-range(flmp,frq,na.rm=TRUE);
        }
        remove(dfr,frq);
        if (!samePlotScale) flim<-flmp;
        if (is.finite(flmp[2])){
            cat("\tPlotting ",y,'\n')
            plot(zlim,flim,type='n',
                 xlim=zlim,     xlab=xlab,
                 ylim=1.05*flim,ylab=ylabp);
#            box(which='inner',fg="light gray")
            usr<-par("usr");
#            print(usr)
            rect(usr[1],usr[3],usr[2],usr[3],col="light gray",border=NA,xpd=TRUE)
            for (i in 1:n){
                dfr<-dfrsp[[i]]$dfr;
                bns<-as.vector(t(dfr[dfr[[yCol]]==y,zCol]));
                frq<-as.vector(t(dfr[dfr[[yCol]]==y,fCol]));
                nb<-length(bns);
                if (nb==0){
                    cat('\t\tDropping',lbls[i],'from plot. No data.\n')
                } else if (nb>0){
                    cat('\t\tIncluding',lbls[i],'in plot. nb=',nb,'\n')
                    if (norm) {frq<-frq/sum(frq);}
                    if (alternate) {frq<-(-1)^(i-1)*frq;}
                    for (ib in 1:(nb-1)){
                        #on.exit(cat('y=',y,'i=',i,'ib=',ib,'frq[ib]=',frq[ib],'\n'));
                        if (frq[ib]!=0){
                            rect(bns[ib],0,bns[ib+1],frq[ib],border=NA,
                                col=wtsUtilities::addTransparency(cols[i],alpha=alpha));
                        }
                    }
                    if (frq[nb]!=0){
                        rect(bns[nb],0,bns[nb]+(bns[nb]-bns[nb-1]),frq[nb],border=NA,
                            col=wtsUtilities::addTransparency(cols[i],alpha=alpha));
                    }
                    lines(bns,frq,lty=ltys[i],col=cols[i],lwd=lwd,type="s");     
                }#nb>0
            }#i loop
            remove(dfr,bns,frq);
            if (!is.null(vlines)){
                abline(v=vlines,lty=2,col=gray(0.2),lwd=2)
            }
            if (plotPDF){
                ctr<-ctr+1;
                mtext(y,side=3,adj=0.05,cex=0.7,line=-1.1)
                if (ctr==1){
                    mtext(xlab,side=1,cex=1,outer=TRUE,line=0.5)
                    mtext(ylab,side=2,cex=1,outer=TRUE,line=0.5)
                    if (!is.null(lbls)){
                        pos<-'left';
                        if (alternate) pos<-'bottomleft'
                        legend(pos,lbls,col=cols,lty=ltys);
                    }
                }
                ctr<-ctr%%(plt.nr*plt.nc);
            } else {
                mtext(paste("year =",y),side=3,adj=0.05)
                if (!is.null(lbls)){
                    legend("topleft",lbls,col=cols,lty=ltys);
                }
            }
        }#finite(fmxp)
    }#y loop
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
