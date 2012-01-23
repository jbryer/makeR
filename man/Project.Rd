\name{Project}
\alias{Project}
\title{Creates a new empty project.}
\usage{
  Project(projectDir = getwd(), name = NULL,
    sourceDir = "source", buildDir = "build",
    releaseDir = "release", sourceFile = ".rnw",
    properties = list())
}
\arguments{
  \item{projectDir}{the root directory of the project.}

  \item{name}{the name of the project. Only used for new
  projects.}

  \item{sourceDir}{the directory containing the source
  files. Only used for new projects.}

  \item{buildDir}{the directory where built versions will
  be located. Only used for new projects.}

  \item{releaseDir}{the directory where released versions
  will be located. Only used for new projects.}

  \item{sourceFile}{the source file to be built. This can
  be a file pattern so that multiple files of the same type
  can be built.}

  \item{properties}{list of global properties for the
  project.}
}
\description{
  TODO: More documentation
}
\details{
  Methods: \tabular{ll}{ build \tab Builds the project.\cr
  rebuild \tab Rebuilds the project without first copying
  the files.\cr save \tab Saves the PROJECT.xml file.\cr
  newVersion \tab Creates a new versions of the project.\cr
  release \tab Releases a version (i.e. copies the built
  file to the releases directory)\cr getProperties \tab
  Returns the project properties.\cr addProperty \tab Adds
  a project property.\cr removeProperty \tab Removes the
  given project property.\cr getReleases \tab Returns a
  list of released files.\cr openRelease \tab Opens the
  given released file with the system's default
  application.\cr }
}

