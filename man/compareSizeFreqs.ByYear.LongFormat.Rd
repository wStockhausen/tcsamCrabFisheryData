\name{compareSizeFreqs.ByYear.LongFormat}
\alias{compareSizeFreqs.ByYear.LongFormat}
\title{Plot multiple size frequencies for comparison by year.}
\usage{
compareSizeFreqs.ByYear.LongFormat(dfrs = NULL, n = 1, lbls = NULL,
  yCol = "year", zCol = "size", fCol = "frequency", norm = TRUE,
  alternate = FALSE, ylim = NULL, zlim = NULL, flim = NULL,
  samePlotScale = TRUE, vlines = c(127, 140), ltys = 1, lwd = 2,
  cols = c("blue", "green", "cyan", "magenta"), alpha = 0.5,
  pdfFile = NULL, plt.nc = 4, plt.nr = 5)
}
\arguments{
  \item{dfrs}{- list of dataframes w/ size frequencies to
  compare}

  \item{n}{- number of size frequency "types" to compare
  (ignored if dfrs is given)}

  \item{lbls}{- vector of labels for size frequency "types"
  to use in plot legend}

  \item{yCol}{- dataframe column name with year info}

  \item{zCol}{- dataframe column name with size info}

  \item{fCol}{- dataframe column name with frequency info}

  \item{norm}{- flag (T/F) to plot normalized size
  frequencies}

  \item{alternate}{- flag (T/F) to alternate
  positive/negative scales for frequencies}

  \item{ylim}{- range of years to plot (can be NULL)}

  \item{zlim}{- range of sizes to plot (can be NULL)}

  \item{flim}{- range for frequency scale on plot (can be
  NULL)}

  \item{samePlotScale}{- flag (T/F) to use same frequency
  scale for all plots}

  \item{vlines}{- sizes at which to plot vertical reference
  lines}

  \item{ltys}{- vector of line types to use to plot size
  freq.s}

  \item{lwd}{- line width for outlines of size freq.s}

  \item{cols}{- vector of colors to use to plot size
  freq.s}

  \item{alpha}{- transparency value (0-1) to use for areas}

  \item{pdfFile}{- name of output pdf file (or NULL, if
  none desired)}

  \item{plt.nc}{- number of columns per page in which to
  arrange plots for pdf output}

  \item{plt.nr}{- number of rows per page in which to
  arrage plots for pdf output}
}
\description{
Function to plot size frequencies by year for visual
comparison.
}
\details{


Dataframes should be derived from "long" format csv files
for annual size frequencies.

If 'dfrs' is NULL, then 'n' represents the number of size
frequency files the user will be prompted to specify for
the comparison.
}

