#'
#'@title Plot maps of ADF&G crab fishery observer data.
#'
#'@description Function to plot maps of ADF&G crab fishery observer data.
#'
#'
#'@details 
#' External Requirements:
#'   * GMT 4.5.x
#'   * on Windows, need both gswin32c and gswin64c installed
#'   * on network, need to use mapped drives to specify files
#'
#' @param   dfr = dataframe or csv file to plot
#' @param   lat = column name containing latitudes
#' @param   lon = column name containing longitudes
#' @param   col = name of column containing z data
#' @param   gmt = GMT version (4 or 5)
#' @param   label = map title
#' @param   year = year label
#' @param   xyrng = x-y range for map as GMT string ('xmin/xmax/ymin/ymax')
#' @param   zscl      = z-scale (max) for map
#' @param   ztype = label for z axes
#' @param   zunits = units for z axes
#' @param   rotate = flag (T/F) or value of angle for map rotation (angle=180 if rotate=T/F)
#' @param   elev = elevation for map perspective is rotate is not FALSE
#' @param   delx = x increment for associated grids
#' @param   dely = y increment for associated grids
#' @param   logtr = flag to ln-transform z data
#' @param   blocktype = flag ('MEAN' or 'SUM') for grouping data 
#' @param   plt_blocktype = flag ('SMOOTH','COARSE') for displaying surface
#' @param   plt_surface = flag to plot data as a color density image
#' @param   plt_blocklocations = flag to plot block locations as X's
#' @param   plt_bars = flag to plot data as bars
#' @param   plt_colorscale = flag to plot color scale
#' @param   plt_reflines = flag to include refernce lines on map
#' @param   reflines = list of lists(lon=,lat=) of reference lines to plot
#' @param   plt_title = flag to include title on map
#' @param   showMap   = flag (T/F) to view EPS plots using GSView
#' @param   psFile = filename for output file (no extension--will be pdf)
#' @param   pdfDir = directory for output file
#' @param   bathymetryfile = filename of bathymetry to plot
#' @param   cleanup = flag to remove temporary files
#'
#'@importFrom wtsUtilities getOperatingSystem
#'@importFrom wtsGMT gridCSV
#'@importFrom wtsGMT plotMap.CSV
#'
#'@export
#'
plotMaps.ObservedCatch<-function(dfr=NULL,
                                 lat='latitude',
                                 lon='longitude',
                                 cols=c('legalret','legalnr','sublegal','female'),
                                 labels=c('retained males','discarded legal males','sublegal males','females'),
                                 ztype='Catch',
                                 zunits='crab',
                                 xyrng="180/205/54/62",
                                 year=NULL,
                                 zscl=NULL,
                                 logtr=FALSE,
                                 noplot=FALSE,
                                 rotate=FALSE,
                                 elev=70,
                                 regrid=TRUE,
                                 delx=0.5,
                                 dely=0.25,
                                 blocktype='SUM',
                                 plt_blocktype='COARSE',
                                 plt_title=TRUE,
                                 plt_bars=TRUE&(rotate!=FALSE),
                                 plt_surface=FALSE,
                                 plt_colorscale=plt_surface|plt_bars,
                                 plt_stations=FALSE,
                                 plt_reflines=TRUE,
                                 reflines=list(list(lon=c(-166,-166),lat=c(50,80))),
                                 showMap=TRUE,
                                 psFile='catchMaps',
                                 pdfDir='',
                                 bathymetryFile=file.path(getwd(),'data/depthcontour_200500.prn'),
                                 cleanup=FALSE) {
  
    
    #check the operating platform
    MacOSX<-'MacOSX';
    Windws<-'Windows';
    platform<-wtsUtilities::getOperatingSystem();
    
    #check for unc paths
    if (platform==Windws){
        if (length(grep('\\\\',getwd()))>0){
            cat("Working dir is using a network path:\n",
                getwd(),'\n',
                'This function uses Windows .bat files.\n',
                'Please use a mapped drive path for the working directory!!\n',
                'Exiting function\n');
            return;
        }
    }
    
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
    
    if (is.null(dfr)) return(NULL);
    if (is.null(year)){
    cat("Returning dataframe. Please run again and supply a year.\n");
    return(dfr);
    }
  
  #extract & regrid catch data
  zmx<-0;
  grds<-list();
  for (col in cols){
    tbl1<-data.frame(longitude=dfr[[lon]],
                     latitude=dfr[[lat]]);
    tbl1[[col]]<-dfr[[col]];
    if (regrid){
      grds[[col]]<-wtsGMT::gridCSV(dfr=tbl1,
                                   lat=lat,
                                   lon=lon,
                                   col=col,
                                   xyrng=xyrng,
                                   delx=delx,
                                   dely=dely,
                                   blocktype=blocktype);
    } else {
      grds[[col]]<-tbl1;
    }
    zmx<-max(zmx,grds[[col]][,col],na.rm=TRUE)
  }
  lat<-'latitude';
  lon<-'longitude';
  
  if (is.null(zscl)) {
    if (logtr) {
      zscl<-0.9*log(zmx+1)/log(10);
    } else {
      zscl<-0.8*zmx;
    }
  }
    
  epsFiles<-vector("character",0);
  for (ic in 1:length(cols)){
    col<-cols[ic];
    label<-labels[ic];
    psfname<-paste(psFile,"_",year,"_",col,sep="")
    cat("Creating files for case '",psfname,"'\n",sep='')
    zsclp<-wtsGMT::plotMap.CSV(grds[[col]],
                               lat=lat,
                               lon=lon,
                               col=col,
                               label=label,
                               year=year,
                               zscl=zscl,
                               ztype=ztype,
                               delx=delx/2,
                               dely=dely,
                               blocktype=blocktype,
                               zunits=zunits,
                               logtr=logtr,
                               rotate=rotate,
                               plt_title=plt_title,
                               plt_bars=plt_bars,
                               plt_surface=plt_surface,
                               plt_colorscale=plt_colorscale,
                               plt_stations=plt_stations,
                               plt_reflines=plt_reflines,
                               reflines=reflines,
                               pdfDir=pdfDir,
                               psFile=psfname,
                               bathymetryFile=bathymetryFile,
                               showMap=showMap)                
    epsFiles[ic]<-paste(psfname,".eps",sep="");
  }    
#   if (!is.null(pdfFile)) {
#     #readline(prompt="Creating pdf file. Hit return: \n")    
#     createPDFfromEPS(pdfFile,epsFiles);
#   }
  return(dfr);
}  

#dfr<-plotMaps.ObservedCatch(noplot=TRUE)


# res<-plotMaps.ObservedCatch(dfr=dfr,year=2009,
#                             blocktype='SUM',plt_blocktype='COARSE',rotate=TRUE,
#                             plt_title=TRUE,plt_stations=TRUE,plt_bars=TRUE,
#                             plt_surface=TRUE,plt_reflines=TRUE);

#res<-plotMaps.ObservedCatch(tbl=dfr,year=2009,plt_stations=TRUE,no_rot=TRUE);
