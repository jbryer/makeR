#' This function will build Sweave (Rnw) files using the cacheSweave driver.
#'
#' @param file the source file to build.
#' @param theenv the environment to build in.
#' @param ... other unspecified parameters
#' @return the name of the file if successfully built.
#' @export
builder.cacheSweave <- function(file, theenv, ...) {
	require(cacheSweave)
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
	return(paste(substr(file, 1, (nchar(file)-4)), '.pdf', sep=''))
}
