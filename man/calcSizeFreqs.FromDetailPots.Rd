\name{calcSizeFreqs.FromDetailPots}
\alias{calcSizeFreqs.FromDetailPots}
\title{Calculate size frequencies from ADF&G detailed "measure pot" data.}
\usage{
calcSizeFreqs.FromDetailPots(tbl = NULL, cutpts = seq(from = 25, to = 185,
  by = 5), truncate.low = FALSE, truncate.high = FALSE, csv.base = NULL)
}
\arguments{
  \item{tbl}{- dataframe derived from old-format ADF&G
  observed size frequencies csv file, or csv file itself}

  \item{cutpts}{- vector of cut points for binning}

  \item{truncate.low}{- flag (T/F) to exclude sizes smaller
  than the 1st cutpoint}

  \item{truncate.high}{- flag (T/F) to exclude sizes larger
  than the last cutpoint}

  \item{csv.base}{- filename base for output csv files}
}
\value{
list with 3 elements * long - size frequencies in long
format * wide - size frquencies in wide format *
sample.size - associated sample sizes
}
\description{
Calculate size frequencies from ADF&G detailed "measure
pot" data.
}
\details{


Calculate size frequencies from ADF&G detailed "measure
pot" data.
}

