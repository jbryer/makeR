\name{Project}
\alias{Project}
\alias{addProperty}
\alias{build}
\alias{getProperties}
\alias{getReleases}
\alias{newVersion}
\alias{openRelease}
\alias{rebuild}
\alias{release}
\alias{removeProperty}
\alias{save}
\title{An R package for managing document versions and templates.}
\usage{
  Project(projectDir = getwd(), name = NULL,
    sourceDir = "source", buildDir = "build",
    releaseDir = "release", sourceFile = NULL,
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
  be a file pattern (e.g. .rnw$) so that multiple files of
  the same type can be built or a single file. The
  comparison will be done ignoring case.}

  \item{properties}{list of global properties for the
  project.}
}
\description{
  The idea of build automation is not new. GNU Make and
  Java Ant are well established and robust build automation
  systems but require the use and installation of
  additional software. The makeR package provides a sim-
  plified framework written entirely in R to manage Sweave
  projects where multiple versions are created based upon a
  single source repository. For example, a monthly report
  where each version is identical, with perhaps the
  exception of easily extracted properties (e.g. date
  ranges for data extraction, title, etc.).
}
\details{
  Most interaction with the \code{makeR} package will occur
  through the \code{Project} class. \code{Project} is an S3
  class that defines some generic functions (e.g. print).
  The \code{Project} class contains a number of properties
  and methods. By convention, properties begin with a
  capital case letter and methods begin with a lower case
  letter. Each is accessedd using the dollar sign notation
  (e.g. \code{myproject$build()}). See below for details
  for each property and method.

  See \code{demo('rbloggers')} for an interactive example.

  Properties: \itemize{ \item \code{BuildDir} - the
  directory where builds will occur.  \item \code{Builds} -
  list of completed builds.  \item \code{CurrentBuild} - an
  integer of the last build.  \item \code{ProjectDir} - the
  base directory where the project is located.  \item
  \code{ProjectFile} - the name of the source file to be
  built.  \item \code{ProjectName} - the name of the
  project.  \item \code{Properties} - a list of the project
  properties. See also \code{p$getProperties()}.  \item
  \code{ReleaseDir} - the directory where released files
  will be located.  \item \code{SourceDir} - the directory
  containing the source files.  \item \code{Versions} - a
  list of the project versions.  \item \code{file.info} -
  the file info from the last time the PROJECT.xml file was
  read. }

  Methods: \itemize{ \item \code{build} Builds the project.
  \code{version} - (optional) the version to build.
  \code{saveEnv} - if TRUE, the build environment (.rda)
  will be saved in the build directory.  \code{builder} -
  the builder function. See also \code{\link{builder.rnw}},
  \code{\link{builder.cacheSweave}},
  \code{\link{builder.tex}} \code{clean} - if TRUE a clean
  build will be performed.  \item \code{rebuild} Rebuilds
  the project without first copying the files.
  \code{version} - (optional) the version to rebuild.
  \code{saveEnv} - if TRUE, the build environment (.rda)
  will be saved in the build directory.  \code{builder} -
  the builder function.See also \code{\link{builder.rnw}},
  \code{\link{builder.cacheSweave}},
  \code{\link{builder.tex}} \item \code{save} Saves the
  PROJECT.xml file.  \item \code{newVersion} Creates a new
  versions of the project.  \code{name} - (optional) the
  version name.  \code{properties} - version specific
  properties.  \item \code{release} Releases a version
  (i.e. copies the built file to the releases directory)
  \code{version} - (optional) the version to release. If
  omitted the latest version will be released.  \item
  \code{getProperties} Returns the project properties.
  \item \code{addProperty} Adds a project property.
  \code{name} - The property name.  \code{value} - The
  property value.  \item \code{removeProperty} Removes the
  given project property.  \code{name} - The property name.
  \item \code{getReleases} Returns a list of released
  files.  \item \code{openRelease} Opens the given released
  file with the system's default application.  \code{file}
  - The released file to open. }
}
\examples{
\dontrun{
myProject = Project(name="RBloggers", projectDir=projectDir, properties=list(email=email, passwd=passwd))
myProject$save()
## Create the first version. This will be for summarizing December 2011 posts.
myProject$newVersion(name='2011-12', properties=list(startDate='2011-12-01', endDate='2011-12-31'))
## Add Project property
myProject$addProperty("author", "Jason Bryer")
## Build the initial version.
myProject$build()
myProject$rebuild(builder=builder.tex, sourceFile='rbloggers.tex')
myProject$Builds ## See that the build completed successfully
## Release the latest version
myProject$release(version='2011-12')
}
}

