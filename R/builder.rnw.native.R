#' Build Sweave files using native calls to pdflatex.
#' 
#' This function will build Sweave (Rnw) files. This function is similar to 
#' \code{\link{builder.rnw}} but instead of calling \code{texti2pdf}, this will 
#' call latex natively instead of using the \code{texi2dvi} function.
#' \code{
#' system('pdflatex FILENAME') 
#' system('bibtex FILENAME')
#' system('pdflatex FILENAME')
#' system('pdflatex FILENAME')
#' }
#'
#' @param project the project to be built.
#' @param theenv the environment to build in.
#' @param fork if true Sweave will be executed in a separate R instance.
#' @param debug debug option sent to the Sweave function. If true, the output
#'        of R code from the Rnw file will be printed as it is running.
#' @param native if not NULL, the shell command to execute to generate
#' @param ... other unspecified parameters
#' @return the name of the file if successfully built.
#' @seealso \code{\link{system}}
#' @export
builder.rnw.native <- function(project, theenv, fork=TRUE, debug=TRUE, ...) {
	sleeptime = 2
	sourceFile = ifelse(is.null(project$SourceFile), ".rnw$", project$SourceFile)
	wd = eval(getwd(), envir = theenv)
	files = list.files(path = wd, pattern = sourceFile, ignore.case = TRUE)
	built = character()
	for(i in seq_len(length(files))) {
		file = files[i]
		message("Running Stangle...\n")
		Stangle(file)
		message("Running Sweave...\n")
		if(fork) {
			envstr = env2string(theenv)
			thecall = paste("Rscript -e \"", envstr, 
						" Sweave('", file, "', debug=", debug, ")\"", sep = "")
			message(paste(thecall, "\n"))
			system(thecall)
		} else {
			for (i in ls(theenv)) {
				assign(i, get(i, envir = theenv), envir=globalenv())
			}
			Sweave(file, debug=debug)
		}
		Sys.sleep(sleeptime)
		texfile = substr(file, 1, (nchar(file) - 4))
		message(paste("Running pdflatex on ", texfile, "...\n", sep=''))
		system(paste('pdflatex ', texfile, '.tex', sep=''))
		system(paste('bibtex ', texfile, sep=''))
		system(paste('pdflatex ', texfile, '.tex', sep=''))
		system(paste('pdflatex ', texfile, '.tex', sep=''))
		built = c(built, paste(substr(file, 1, (nchar(file) - 4)), ".pdf", sep = ""))
		Sys.sleep(sleeptime)
	}
	return(built)
}
