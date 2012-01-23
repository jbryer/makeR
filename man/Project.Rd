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

