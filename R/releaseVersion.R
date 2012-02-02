#' This is an internal method and should not be called directly.
#'
#' Release the latest built version.
#'
#' @param pv the Project
#' @param major the major version number or name that should be released.
#' @param increment should the minor version number be incremented.
#' @param ... other non-specified parameters
releaseVersion <- function(pv, major=NULL, increment=TRUE, ...) {
	wd = setwd(pv$ProjectDir)
	
	versionPosition = NULL	
	version = NULL
	if(!is.null(major)) {
		if(is.numeric(major)) {
			version = pv$Versions[[major]]
			versionPosition = major
		} else {
			n = unname(unlist(lapply(pv$Versions, FUN=function(x, ...) { x[['Name']] } )))
			versionPosition = which(n == major)
			version = pv$Versions[[versionPosition]]
			if(is.null(version)) {
				stop(paste("Could not find version ", major, sep=''))
			}
		}
	} else {
		versionPosition = length(pv$Versions)
		version = pv$Versions[[versionPosition]]
	}
	
	major = NULL
	if(!is.null(version$Name)) {
		major = version$Name
	} else {
		major = version$Major
	}
	
	releaseDir = paste(pv$ProjectDir, '/', pv$ReleaseDir, '/', sep='')
	dir.create(releaseDir, recursive=TRUE, showWarnings=FALSE)
	
	for(i in length(pv$Builds):1) {
		if(pv$Builds[[i]]$Major == major | pv$Builds[[i]]$Name == major) {
			build = pv$Builds[[i]]
			break()
		}
	}
	buildNum = build$Build
	minorNum = build$Minor
	for(i in seq_len(length(build$Files))) {
		filename = build$Files[i]
		parts = unlist(strsplit(filename, '\\.'))
		base = paste(parts[1:(length(parts)-1)], sep='.')
		ext = parts[length(parts)]
		fromFile = paste(pv$BuildDir, '/', major, '.', version$Minor, '/', filename, sep='')
		toFile = paste(pv$ReleaseDir, '/', base, '-', major, '.', minorNum, '.', ext, sep='')
		cat(paste('Copying', fromFile, 'to', toFile))
		file.copy(fromFile, toFile)
	}
	
	if(increment) {
		#Increment the minor version number
		versions = pv$Versions
		versions[[versionPosition]]$Minor = as.numeric(versions[[versionPosition]]$Minor) + 1
		assign("Versions", versions, envir=pv)
	}
	
	if(isAutoOpen()) {
		try(system(paste("open \"", pv$ProjectDir, "/", toFile, "\"", sep='')))
	}
	if(!is.null(wd)) setwd(wd)
	invisible(pv)
}
