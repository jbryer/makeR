#'
#'
#' @export
addProperty <- function(x, name, value) {
	UseMethod("addProperty")
}
#'
#'
#' @export
addProperty.default <- addProperty

#'
#'
#' @export
addProperty.Version <- function(x, name, value) {
	x$properties[[name]] = value
	return(x)
}

#'
#'
#' @export
addProperty.Project <- function(x, name, value) {
	x$properties[[name]] = value
	if(`_AUTOSAVE`) {
		write.Project(x)
	}
	return(x)
}

#'
#'
#' @export
removeProperty <- function(x, name) {
	UseMethod("removeProperty")
}

#'
#'
#' @export
removeProperty.default <- removeProperty

#'
#'
#' @export
removeProperty.Version <- function(x, name) {
	x$properties[[name]] = NULL
	return(x)
}

#'
#'
#' @export
removeProperty.Project <- function(x, name) {
	x$properties[[name]] = NULL
	if(`_AUTOSAVE`) {
		write.Project(x)
	}
	return(x)
}
