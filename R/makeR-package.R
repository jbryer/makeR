#' Package for managing projects with multiple versions derived from a single source repository.
#'
#' An R package to help manage R projects (e.g. Sweave reports) where multiple 
#' versions are created based upon a single source repository. For example, a 
#' monthly report where each versions is identitcal with the exception of easily 
#' definable parameters (e.g. date ranges for data extraction, title, etc.).
#' 
#' @name makeR-package
#' @docType package
#' @title Package for managing projects with multiple versions derived from a
#'        single source repository.
#' @author \email{jason@@bryer.org}
#' @keywords versioning templating Sweave
#' @import tools XML
NULL

.onLoad <- function(libname, pkgname) {
}

AUTOSAVE <- TRUE
AUTOOPEN <- TRUE
DEFAULT_BUILDER <- NULL

.onAttach <- function(libname, pkgname) {
	pkgEnv = pos.to.env(match('package:makeR', search()))
	packageStartupMessage(paste("Auto saving of PROJECT.xml is ", 
			  ifelse(makeR:::AUTOOPEN, "enabled", "disabled"),
			  ". This can be changed using the setAutoSave() function.\n", sep=''))
	packageStartupMessage(paste("Auto opening of built and released files is ", 
			  ifelse(makeR:::AUTOOPEN, "enabled", "disabled"),
			  ". This can be changed using the setAutoOpen() function.\n", sep=''))
	setDefaultBuilder(builder.rnw)
}

#' Sets whether the PROJECT.xml file is automatically saved when project properties
#' have changed.
#'
#' @seealso \code{Project$save()}
#' @param value if TRUE PROJECT.xml will be saved automitcally.
#' @export
setAutoSave <- function(value) {
	assignInNamespace("AUTOSAVE", value, "makeR")
}

#' Returns whether the PROJECT.xml file is automatically saved.
#'
#' @seealso \code{\link{setAutoSave}}
#' @export
isAutoSave <- function() {
	return(makeR:::AUTOSAVE)
}

#' Sets whether built or released files will be opened automatically using the
#' system's default PDF viewer.
#'
#' @param value if TRUE PDF files will be opened automatically.
#' @export
setAutoOpen <- function(value) {
	assignInNamespace("AUTOOPEN", value, "makeR")}

#' Returns whether build or released files will be opened automaticall.
#'
#' @seealso \code{\link{setAutoOpen}}
#' @export
isAutoOpen <- function() {
	return(makeR:::AUTOOPEN)
}

#' Sets the default builder if none is specified to \code{Project$build}.
#'
#' @param value the function to use as the default builder.
#' @seealso \code{\link{Project}}, \code{\link{builder.rnw}}, \code{\link{builder.tex}},
#'          \code{\link{builder.cacheSweave}}, \code{\link{builder.knitr}}
#' @export
setDefaultBuilder <- function(value) {
	assignInNamespace("DEFAULT_BUILDER", value, "makeR")
}

#' Returns the default bulder if none is specified to \code{Project$build}.
#'
#' @seealso \code{\link{Project}}
#' @export
getDefaultBuilder <- function() {
	return(makeR:::DEFAULT_BUILDER)
}

