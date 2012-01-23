#' This function will build Sweave (Rnw) files. Specifically this function will
#' first run Stangle, then Sweave, and finally texti2pdf.
#'
#' @param file the source file to build.
#' @param ... other unspecified parameters
#' @return the name of the file if successfully built.
#' @export
builder.tex <- function(file, ...) {
	cat('Running texi2dvi...\n')
	texi2pdf(file)
	return(paste(substr(file, 1, (nchar(file)-4)), '.pdf', sep=''))
}
