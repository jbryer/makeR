#' This function will convert the elements in the given environment to a single
#' character string. This is useful when passing the contents of the environment
#' to a new R instances vis-a-vis the command line.
#'
#' @param theenv the environment to convert to a string.
#' @return the string representation of the environment.
#' @export
env2string <- function(theenv) {
	st <- character()
	for(i in 1:length(ls(theenv))) {
		varName <- ls(theenv)[i]
		varValue <- get(varName, theenv)
		if(class(varValue) == 'character') {
			varValue <- paste("'", varValue, "'", sep='')
		} else if(class(varValue) == 'function') {
			varValue <- paste(deparse(varValue), collapse='')
		}
		st = paste(st, varName, '=', varValue, '; ', sep='')
	}
	return(st)
}
