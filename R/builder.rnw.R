#' This function will build Sweave (Rnw) files. Specifically this function will
#' first run Stangle, then Sweave, and finally texti2pdf.
#'
#' @param file the source file to build.
#' @param theenv the environment to build in.
#' @param ... other unspecified parameters
#' @return the name of the file if successfully built.
#' @export
builder.rnw <- function(file, theenv, ...) {
	cat('Running Stangle...\n')
	Stangle(file)
	cat('Running Sweave...\n')
	envstr = env2string(theenv)
	thecall = paste('Rscript -e "', envstr, ' Sweave(\'', file, '\')"', sep='')
	cat(paste(thecall, '\n'))
	system(thecall)
	cat('Running texi2dvi...\n')
	texi2pdf(paste(substr(file, 1, (nchar(file)-4)), '.tex', sep=''))
	return(paste(substr(file, 1, (nchar(file)-4)), '.pdf', sep=''))
}
