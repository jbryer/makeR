#' This function will documents using \code{knitr}. Since \code{knitr} is flexible
#' enough to support multiple file types (e.g. Sweave, HTML, Markdown, etc.), specifying
#' \code{Project$SourceFile} and \code{output} (see \code{\link[knitr:knit]{knit}})
#' is necessary for file types other than Sweave (i.e. .rnw files).
#'
#' @param project the project to be built.
#' @param theenv the environment to build in.
#' @param ... other parameters passed to \code{\link[knitr]{knit}} and \code{\link[knitr:knit]{curl}}
#' @return the name of the file if successfully built.
#' @export
builder.knitr <- function(project, theenv, ...) {
	sleeptime = 2
	sourceFile = ifelse(is.null(project$SourceFile), ".rnw$", project$SourceFile)
	wd = eval(getwd(), envir = theenv)
	files = list.files(path = wd, pattern = sourceFile, ignore.case = TRUE)
	built = character()
	for(i in seq_len(length(files))) {
		file = files[i]
		message("Running purl...\n")
		purl(file, ...)
		message("Running knit...\n")
		for (i in ls(theenv)) {
			assign(i, get(i, envir = theenv), envir=globalenv())
		}
		knit(file, ...)
		Sys.sleep(sleeptime)
		texfile = paste(substr(file, 1, (nchar(file) - 4)), ".tex", sep = "")
		built = NULL
		if(exists('output')) {
			built = output
		} else if(file.exists(texfile)) {
			message(paste("Running texi2pdf on ", texfile, "...\n", sep=''))
			texi2pdf(texfile, quiet=FALSE)
			built = c(built, paste(substr(file, 1, (nchar(file) - 4)), ".pdf", sep = ""))
		} else {
			warning('Unknown output file. Please specify output parameter.')
		}
		Sys.sleep(sleeptime)
	}
	return(built)
}
