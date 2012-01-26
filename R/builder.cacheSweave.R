#' This function will build Sweave (Rnw) files using the cacheSweave driver.
#'
#' @param file the source file to build.
#' @param theenv the environment to build in.
#' @param ... other unspecified parameters
#' @return the name of the file if successfully built.
#' @export
builder.cacheSweave <- function(sourceFile, theenv, ...) {
	require(cacheSweave)
	if(is.null(sourceFile)) { sourceFile = ".rnw" }
	wd = eval(getwd(), envir=theenv)
	files = list.files(path=wd, pattern=sourceFile, ignore.case=TRUE)
	built = character()
	for(i in seq_len(length(files))) {
		file = files[i]
		cat('Running Stangle...\n')
		Stangle(file)
		cat('Running Sweave with cacheSweave...\n')
		envstr = env2string(theenv)
		thecall = paste('Rscript -e "require(cacheSweave); ', envstr, 
						' Sweave(\'', file, '\', driver=cacheSweaveDriver)"', sep='')
		cat(paste(thecall, '\n'))
		system(thecall)
		cat('Running texi2dvi...\n')
		texi2pdf(paste(substr(file, 1, (nchar(file)-4)), '.tex', sep=''))
		built = c(built, paste(substr(file, 1, (nchar(file)-4)), '.pdf', sep=''))
	}
	return(built)
}
