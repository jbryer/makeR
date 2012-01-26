#' This function will build LaTeX (tex) files using texti2pdf.
#'
#' @param project the project to be built.
#' @param theenv the environment to build in.
#' @param ... other unspecified parameters
#' @return the name of the file if successfully built.
#' @export
builder.tex <- function(project, theenv, ...) {
	sourceFile = ifelse(is.null(project$SourceFile), '.rnw$', project$SourceFile)
	wd = eval(getwd(), envir=theenv)
	files = list.files(path=wd, pattern=sourceFile, ignore.case=TRUE)
	built = character()
	cat('Running texi2dvi...\n')
	for(i in seq_len(length(files))) {
		file = files[i]
		texi2pdf(file)
		built = c(built, paste(substr(file, 1, (nchar(file)-4)), '.pdf', sep=''))
	}
	return(built)
}
