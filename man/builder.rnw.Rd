\name{builder.rnw}
\alias{builder.rnw}
\title{This function will build Sweave (Rnw) files. Specifically this function will
first run Stangle, then Sweave, and finally texti2pdf.}
\usage{
  builder.rnw(file, theenv, ...)
}
\arguments{
  \item{file}{the source file to build.}

  \item{theenv}{the environment to build in.}

  \item{...}{other unspecified parameters}
}
\value{
  the name of the file if successfully built.
}
\description{
  This function will build Sweave (Rnw) files. Specifically
  this function will first run Stangle, then Sweave, and
  finally texti2pdf.
}

