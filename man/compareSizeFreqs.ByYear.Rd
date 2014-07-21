\name{compareSizeFreqs.ByYear}
\alias{compareSizeFreqs.ByYear}
\title{Plot multiple size frequencies for comparison by year.}
\usage{
compareSizeFreqs.ByYear(dfrs = NULL, n = 1, lbls = NULL, norm = TRUE,
  vlines = c(127, 140), ltys = 1:4, cols = c("blue", "green", "cyan",
  "gray"), plotPDF = FALSE, nc = 4, nr = 5, pdfFile = "compZFs.pdf")
}
\arguments{
  \item{dfrs}{- list of dataframes w/ size frequencies to
  compare}

  \item{n}{- number of size frequency "types" to compare
  (ignored if dfrs is given)}

  \item{lbls}{- vector of labels for size frequency "types"
  to use in plot legend}

  \item{norm}{- flag (T/F) to plot normalized size
  frequencies}

  \item{vlines}{- sizes at which to plot vertical reference
  lines}

  \item{ltys}{- vector of line types to use to plot size
  freq.s}

  \item{cols}{- vector of colors to use to plot size
  freq.s}

  \item{plotPDF}{- flag (T/F) to output plots to a pdf
  file}

  \item{nc}{- number of columns per page in which to
  arrange plots for pdf output}

  \item{nr}{- number of rows per page in which to arrage
  plots for pdf output}

  \item{pdfFile}{- name of output pdf file}
}
\description{
Function to plot visual comparisons of size frequencies by
year.
}
\details{


Dataframes should be derived from "wide" format csv files
for size frequencies with year as first column and
subsequent columns containing the size frequencies.

If 'dfrs' is NULL, then 'n' represents the number of size
frequency files the user will be prompted to specify for
the comparison.
}

