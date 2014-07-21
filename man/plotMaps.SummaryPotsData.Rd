\name{plotMaps.SummaryPotsData}
\alias{plotMaps.SummaryPotsData}
\title{Plot aggregated data from ADF&G "all pots" (SummaryPots) data files.}
\usage{
plotMaps.SummaryPotsData(tbl = NULL, plotTypes = c("vessels", "pots",
  "female", "sublegal", "legal"), zscls = NULL, byQuarter = FALSE,
  byFishery = FALSE, cleanup = TRUE)
}
\arguments{
  \item{tbl}{- dataframe with aggregated data derived from
  "all pots" (SummaryPots) csv file, or csv file itself}

  \item{plotTypes}{- vector of data types to plot
  {'vessels','pots','female','sublegal','legal'}}

  \item{zscls}{- NULL or list w/ plot types as names.
  Associated values will be used as z-scales.}

  \item{byQuarter}{- flag to plot maps by fishery quarter}

  \item{byFishery}{- flag to plot maps by fishery type}

  \item{cleanup}{- flag to delete temporary files}
}
\value{
list with z-scales for each plot type.
}
\description{
Function that will plot aggregated data from ADF&G "all
pots" (SummaryPots) data files.
}
\details{


Plots maps.
}

