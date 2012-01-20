\name{calendarHeat}
\alias{calendarHeat}
\title{An R function to display time-series data as a calendar heatmap}
\usage{
  calendarHeat(dates, values, ncolors = 99, color = "r2g",
    varname = "Values", date.form = "\%Y-\%m-\%d", ...)
}
\arguments{
  \item{dates}{vector of Dates}

  \item{values}{vector of values}

  \item{ncolors}{number of colors to use}

  \item{color}{the color scheme to use. Currently supports
  r2b, (red to blue), r2g (red to green), and w2b (white to
  blue).}

  \item{varname}{varaible names}

  \item{date.form}{the format of the Date column}

  \item{...}{other non-specified parameters}
}
\description{
  This graphic originally appeared
  \href{http://stat-computing.org/dataexpo/2009/posters/wicklin-allison.pdf}{here}.
  This function is included with the \code{makeR} package
  to support the R-Bloggers demo. See \code{demo('makeR')}
  for more information.
}
\author{
  Paul Bleicher
}

