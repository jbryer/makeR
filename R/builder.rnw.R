#' This function will build Sweave (Rnw) files. Specifically this function will
#' first run Stangle, then Sweave, and finally texti2pdf.
#'
#' @param project the project to be built.
#' @param theenv the environment to build in.
#' @param fork if true Sweave will be executed in a separate R instance.
#' @param ... other unspecified parameters
#' @return the name of the file if successfully built.
#' @export
builder.rnw <- function(project, theenv, fork=TRUE, ...) {
	sleeptime = 2
	sourceFile = ifelse(is.null(project$SourceFile), ".rnw$", project$SourceFile)
	wd = eval(getwd(), envir = theenv)
	files = list.files(path = wd, pattern = sourceFile, ignore.case = TRUE)
	built = character()
	for(i in seq_len(length(files))) {
		file = files[i]
		cat("Running Stangle...\n")
		Stangle(file)
		cat("Running Sweave...\n")
		if(fork) {
			envstr = env2string(theenv)
			thecall = paste("Rscript -e \"", envstr, " Sweave('", file, "')\"", sep = "")
			cat(paste(thecall, "\n"))
			system(thecall)
		} else {
			for (i in ls(theenv)) {
				assign(i, get(i, envir = theenv))
			}
			Sweave(file)
		}
		Sys.sleep(sleeptime)
		texfile = paste(substr(file, 1, (nchar(file) - 4)), ".tex", sep = "")
		cat(paste("Running texi2pdf on ", texfile, "...\n", sep=''))
		texi2pdf(texfile, quiet=FALSE)
		built = c(built, paste(substr(file, 1, (nchar(file) - 4)), ".pdf", sep = ""))
		Sys.sleep(sleeptime)
	}
	return(built)
}
