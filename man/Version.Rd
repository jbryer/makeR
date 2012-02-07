\name{Version}
\alias{Version}
\title{This is an internal method and should not be called directly.}
\usage{
  Version(pv, name = NA, properties = list(), xml = NULL)
}
\arguments{
  \item{pv}{the Project}

  \item{name}{the name of the version}

  \item{properties}{list of version specific properties.}

  \item{xml}{the XML branch if creating from the
  PROJECT.xml file.}
}
\description{
  Constructor for a Version class. This is an interal
  method that builds a Version class from XML. A Version
  has the following methods:
}
\details{
  \itemize{ \item \code{getProperties} Returns the Version
  properties.  \item \code{addProperty} Adds a Version
  property.  \code{name} - The property name.  \code{value}
  - The property value.  \item \code{removeProperty}
  Removes the given Version property.  \code{name} - The
  property name.  \item \code{assignProperties} - Sets the
  project and version properties.  \code{theenv} - the
  envirnment to which the properties will be assigned.
  This defaults to the .GlobalEnv. }
}

