#'
#'
#' @export
addProperty <- function(x, name, value) {
	UseMethod("addProperty")
}

#'
#'
#' @export
addProperty.Version <- function(x, name, value) {
	
}

#'
#'
#' @export
addProperty.Project <- function(x, name, value) {
	
}

#'
#'
#' @export
removeProperty <- function(x, name, value) {
	UseMethod("removeProperty")
}

#'
#'
#' @export
removeProperty.Version <- function(x, name, value) {
	
}

#'
#'
#' @export
removeProperty.Project <- function(x, name, value) {
	
}
