\name{rebinSizeFreqs.NewFormat}
\alias{rebinSizeFreqs.NewFormat}
\title{Re-bin and export size frequencies based on a csv file with annual (new format) ADF&G 1-mm size bin counts.}
\usage{
rebinSizeFreqs.NewFormat(dfr = NULL, aggByRegion = TRUE, aggBySex = FALSE,
  aggByShellCondition = FALSE, cutpts = seq(from = 25, to = 185, by = 5),
  truncate.low = TRUE, truncate.high = FALSE, save = TRUE,
  out.dir = NULL, out.csv = NULL)
}
\arguments{
  \item{dfr}{- dataframe with raw size freq.s (or csv
  filename to read, or NULL for prompt)}

  \item{aggByRegion}{- flag (T/F) to aggregate size comps
  by region}

  \item{aggBySex}{- flag (T/F) to aggregate size comps by
  sex}

  \item{aggByShellCondition}{- flag (T/F) to aggregate size
  comps by shell condition}

  \item{cutpts}{- vector of cut points for re-binned size
  freq.s}

  \item{truncate.low}{- flag (T/F) to exclude sizes smaller
  than the 1st cutpoint}

  \item{truncate.high}{- flag (T/F) to exclude sizes larger
  than the last cutpoint}
}
\value{
A dataframe containing the re-binned size frequencies.
}
\description{
Function to re-bin and export size frequencies based on
annual ADF&G 1-mm size bin counts as csv files.
}
\details{


Input dataframe/csv should have columns
year,region,sex,shellcondition,size,frequency
}

