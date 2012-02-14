\name{buildVersion}
\alias{buildVersion}
\title{This is an internal method and should not be called directly.}
\usage{
  buildVersion(pv, version.major = NULL, saveEnv = TRUE,
    builder = getDefaultBuilder(), clean = FALSE,
    sourceFile = pv$SourceFile, ...)
}
\arguments{
  \item{pv}{the Project}

  \item{version.major}{the version that should be built.
  This can be either the the major number or version name.}

  \item{saveEnv}{whether to save build environment as an
  Rda image file.}

  \item{builder}{the builder function to use.}

  \item{clean}{if TRUE all files in the build directory
  will be deleted before building.}

  \item{sourceFile}{the name of the source file to build.}

  \item{...}{other non-specified parameters}
}
\description{
  Builds a new version of a project. See Project$build
}

