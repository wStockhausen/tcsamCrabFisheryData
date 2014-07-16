#'
#'@title Plot multiple size frequencies for comparison by year.
#'
#'@description Function to plot visual comparisons of size frequencies by year.
#'
#'@param dfrs - list of dataframes w/ size frequencies to compare
#'@param n - number of size frequency "types" to compare (ignored if dfrs is given)
#'@param lbls - vector of labels for size frequency "types" to use in plot legend
#'@param norm - flag (T/F) to plot normalized size frequencies
#'@param vlines - sizes at which to plot vertical reference lines
#'@param ltys - vector of line types to use to plot size freq.s
#'@param cols - vector of colors to use to plot size freq.s
#'@param plotPDF - flag (T/F) to output plots to a pdf file
#'@param nc - number of columns per page in which to arrange plots for pdf output
#'@param nr - number of rows per page in which to arrage plots for pdf output 
#'@param pdfFile - name of output pdf file
#'
#'@details 
#'Dataframes should be derived from "wide" format csv files for size frequencies
#'with year as first column and subsequent columns containing the size frequencies.
#'
#'If 'dfrs' is NULL, then 'n' represents the number of size frequency files the user will
#'be prompted to specify for the comparison. 
#'
#'@import wtsUtilities
#'@import tcltk
#'
#'@export
#'
compareSizeFreqs.ByYear<-function(dfrs=NULL,
                                   n=1,
                                   lbls=NULL,
                                   norm=TRUE,
                                   vlines=c(127,140),
                                   ltys=1:4,
                                   cols=c("blue","green","cyan","gray"),
                                   plotPDF=FALSE,
                                   nc=4,
                                   nr=5,
                                   pdfFile="compZFs.pdf"){
    
    ymn<- NA;
    ymx<- NA;
    bmx<- NA;
    if (is.null(dfrs)){
        flg<-TRUE;
    } else {
        flg<-FALSE;
        n<-length(dfrs);
    }
    
    ltys<-rep(ltys,length.out=n);
    cols<-rep(cols,length.out=n);
    
    csv<-"";
    dfrsp<-vector(mode='list',length=n)
    for (i in 1:n) {
            dfr<-wtsUtilities::getCSV(caption=paste("Select ",i,"th size frequency csv file",sep=""))
            if (is.null(dfr)) {return(NULL);} #user cancelled
        } else {
        if (flg){
            dfr<-dfrs[[i]];
        }
        nc<-ncol(dfr);
        yrs<-dfr$year;
        rownames(dfr)<-yrs;
        bins<-as.numeric(substr(colnames(dfr)[2:nc],2,999));
        ymn<-max(ymn,min(yrs),na.rm=TRUE);
        ymx<-min(ymx,max(yrs),na.rm=TRUE);
        bmx<-max(bmx,bins,na.rm=TRUE);
        dfrsp[[i]]<-list(nc=nc,yrs=yrs,bins=bins,dfr=dfr[,2:nc]);
    }
    
    if (plotPDF){
        par.old<-par(mfrow=c(nr,nc),mai=c(0.0, 0.0, 0.0, 0.0),cex=.5,omi=c(0.5,0.5,0.5,0.5))
        on.exit(par(par.old));
        dev<-pdf(file=pdfFile,8.5,11);
    }
    normf<-1;
    for (y in ymn:ymx){
        mxz<--Inf;
        for (i in 1:n){
            nz<-as.vector(t(dfrsp[[i]]$dfr[as.character(y),]));
            if (norm) {normf<-sum(nz);}
            nz<-nz/normf;
            mxz<-max(mxz,nz,na.rm=TRUE);
        }
        if (is.finite(mxz)){
            plot(c(0,bmx),c(0,mxz),type='n',
                 xlim=c(0,bmx),xlab='size bin (mm)',
                 ylim=1.05*c(0,mxz),ylab="normalized frequency");
            rect(0,0,bmx,1.05*mxz,col="light gray",border=NA)
            for (i in 1:n){
                nz<-as.vector(t(dfrsp[[i]]$dfr[as.character(y),]));
                if (norm) {normf<-sum(nz);}
                nz<-nz/normf;
                lines(dfrsp[[i]]$bins,nz,lty=ltys[i],col=cols[i],lwd=3,type="s")
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

#compareSizeFreqs.ByYear(n=2,lbls=c("revised","2013"),plotPDF="TRUE")

#compareSizeFreqs(n=2,lbls=c("new shell","old shell"),norm=FALSE)
