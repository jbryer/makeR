\name{builder.cacheSweave}
\alias{builder.cacheSweave}
\title{This function will build Sweave (Rnw) files using the cacheSweave driver.}
\usage{
  builder.cacheSweave(project, theenv, ...)
}
\arguments{
  \item{project}{the project to be built.}

  \item{theenv}{the environment to build in.}

  \item{fork}{if true Sweave will be executed in a separate
  R instance.}

  \item{...}{other unspecified parameters}
}
\value{
  the name of the file if successfully built.
}
\description{
  This function will build Sweave (Rnw) files using the
  cacheSweave driver.
}

