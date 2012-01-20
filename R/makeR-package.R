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

`_AUTOSAVE` <- TRUE
`_AUTOOPEN` <- TRUE

.onLoad <- function(libname, pkgname) {
	cat(paste("Auto saving of PROJECT.xml is ", 
			  ifelse(`_AUTOSAVE`, "enabled", "disabled"),
			  ". This can be changed using the setAutoSave() function.\n", sep=''))
	cat(paste("Auto opening of built and released files is ", 
			  ifelse(`_AUTOOPEN`, "enabled", "disabled"),
			  ". This can be changed using the setAutoOpen() function.\n", sep=''))
}

#' Sets whether the PROJECT.xml files is automatically saved when project properties
#' have changed.
#'
#' @seealso \code{Project$save()}
#' @param value if TRUE PROJECT.xml will be saved automitcally.
#' @export
setAutoSave <- function(value) {
	`_AUTOSAVE` <- value
}

#' Sets whether built or released files will be opened automatically using the
#' system's default PDF viewer.
#'
#' @param value if TRUE PDF files will be opened automatically.
#' @export
setAutoOpen <- function(value) {
	`_AUTOOPEN` <- value
}
