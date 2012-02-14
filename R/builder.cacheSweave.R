#' This function will build Sweave (Rnw) files using the cacheSweave driver.
#'
#' @param project the project to be built.
#' @param theenv the environment to build in.
#' @param fork if true Sweave will be executed in a separate R instance.
#' @param debug debug option sent to the Sweave function. If true, the output
#'        of R code from the Rnw file will be printed as it is running.
#' @param ... other unspecified parameters
#' @return the name of the file if successfully built.
#' @export
builder.cacheSweave <- function(project, theenv, fork=TRUE, debug=TRUE, ...) {
	require(cacheSweave)
	sourceFile = ifelse(is.null(project$SourceFile), '.rnw$', project$SourceFile)
	wd = eval(getwd(), envir=theenv)
	files = list.files(path=wd, pattern=sourceFile, ignore.case=TRUE)
	built = character()
	for(i in seq_len(length(files))) {
		file = files[i]
		message('Running Stangle...\n')
		Stangle(file)
		message('Running Sweave with cacheSweave...\n')
		if(fork) {
			envstr = env2string(theenv)
			thecall = paste('Rscript -e "require(cacheSweave); ', envstr, 
							' Sweave(\'', file, '\', driver=cacheSweaveDriver, debug=', 
							debug, ')"', sep='')
			message(paste(thecall, '\n'))
			system(thecall)
		} else {
			for(i in ls(theenv)) { assign(i, get(i, envir=theenv), envir=globalenv()) }
			Sweave(file, driver=cacheSweaveDriver, debug=debug)
		}
		message('Running texi2dvi...\n')
		texi2pdf(paste(substr(file, 1, (nchar(file)-4)), '.tex', sep=''))
		built = c(built, paste(substr(file, 1, (nchar(file)-4)), '.pdf', sep=''))
	}
	return(built)
}
