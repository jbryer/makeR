\name{Build}
\alias{Build}
\title{This is an internal method and should not be called directly.}
\usage{
  Build(buildXML = NULL, buildNum = NULL, major = NULL,
    minor = NULL, name = NULL, timestamp = date(),
    R = R.version$version.string,
    platform = R.version$platform,
    user = Sys.info()["user"],
    nodename = Sys.info()["nodename"], files = NULL)
}
\arguments{
  \item{buildXML}{an XML element.}

  \item{buildNum}{new build number.}

  \item{major}{major version number}

  \item{minor}{minor version number}

  \item{name}{the name of the version}

  \item{timestamp}{the time the file was built}

  \item{R}{the R version information}

  \item{platform}{the platform the file was built}

  \item{user}{the user that performed the build}

  \item{nodename}{the name of the node on which this file
  was built}

  \item{files}{the name of the files that were built. This
  is typically a list of PDFs.}
}
\description{
  Constructor for a Build class. This is an interal method
  used to create a Build class/object.
}

