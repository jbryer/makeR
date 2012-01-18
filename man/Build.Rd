\name{Build}
\alias{Build}
\title{Constructor for a Build class.}
\usage{
  Build(buildXML = NULL, buildNum = NULL, major = NULL,
    minor = NULL, name = NULL, timestamp = date(),
    R = R.version$version.string,
    platform = R.version$platform,
    user = Sys.info()["user"],
    nodename = Sys.info()["nodename"], file = NULL)
}
\arguments{
  \item{buildXML}{an XML element.}
}
\description{
  TODO: Need more documentation
}

