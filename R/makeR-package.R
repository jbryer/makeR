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

.onAttach <- function(libname, pkgname) {
	eval(`_makeR.AUTOSAVE` <<- TRUE, globalenv())
	eval(`_makeR.AUTOOPEN` <<- TRUE, globalenv())
	cat(paste("Auto saving of PROJECT.xml is ", 
			  ifelse(eval(`_makeR.AUTOSAVE`, globalenv()), "enabled", "disabled"),
			  ". This can be changed using the setAutoSave() function.\n", sep=''))
	cat(paste("Auto opening of built and released files is ", 
			  ifelse(eval(`_makeR.AUTOOPEN`, globalenv()), "enabled", "disabled"),
			  ". This can be changed using the setAutoOpen() function.\n", sep=''))
}

#' Sets whether the PROJECT.xml file is automatically saved when project properties
#' have changed.
#'
#' @seealso \code{Project$save()}
#' @param value if TRUE PROJECT.xml will be saved automitcally.
#' @export
setAutoSave <- function(value) {
	eval(`_makeR.AUTOSAVE` <<- value, globalenv())
}

#' Returns whether the PROJECT.xml file is automatically saved.
#'
#' @seealso \code{\link{setAutoSave}}
#' @export
isAutoSave <- function() {
	return(eval(`_makeR.AUTOSAVE`, globalenv()))
}

#' Sets whether built or released files will be opened automatically using the
#' system's default PDF viewer.
#'
#' @param value if TRUE PDF files will be opened automatically.
#' @export
setAutoOpen <- function(value) {
	eval(`_makeR.AUTOOPEN` <<- value, globalenv())
}

#' Returns whether build or released files will be opened automaticall.
#'
#' @seealso \code{\link{setAutoOpen}}
#' @export
isAutoOpen <- function() {
	return(eval(`_makeR.AUTOOPEN`, globalenv()))
}
