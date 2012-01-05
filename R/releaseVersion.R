#' Release the latest built version.
#'
#' TODO: Need more documentation 
#'
#' @export
releaseVersion <- function(pv, major=NULL, rebuild=FALSE, increment=TRUE, ...) {
	wd = getwd()
	
	if(rebuild) {
		buildDoc(pv, build=build, ...)
	}
	
	versionPosition = NULL	
	if(!is.null(major)) {
		if(is.numeric(major)) {
			version = pv$versions[[major]]
			versionPosition = major
		} else {
			version = NULL
			for(i in length(pv$versions):1) {
				if(pv$versions[[i]]$name == major) {
					version = pv$versions[[i]]
					versionPosition = i
				}
			}
			if(is.null(version)) {
				stop(paste("Could not find version ", major, sep=''))
			}
		}
	} else {
		versionPosition = length(pv$versions)
		version = pv$versions[[versionPosition]]
	}
	
	if(!is.null(version$name)) {
		major = version$name
	} else {
		major = version$major
	}
	
	releaseDir = paste(pv$ProjectDir, '/', pv$releaseDir, '/', sep='')
	dir.create(releaseDir, recursive=TRUE, showWarnings=FALSE)
	
	for(i in length(pv$builds):1) {
		if(pv$builds[[i]]$major == major | pv$builds[[i]]$name == major) {
			build = pv$builds[[i]]
			buildNum = pv$builds[[i]]$build
			break()
		}
	}
	
	filename = build$file
	minorNum = build$minor
	
	fromFile = paste(pv$buildDir, '/', major, '.', version$minor, '-', buildNum, '/', filename, sep='')
	toFile = paste(pv$releaseDir, '/', substr(filename, 1, (nchar(filename)-4)), '-', 
				   major, '.', minorNum, '.', buildNum, '.pdf', sep='')
	cat(paste('Copying', fromFile, 'to', toFile))
	file.copy(fromFile, toFile)
	
	if(increment) {
		#Increment the minor version number
		pv$versions[[versionPosition]]$minor = as.numeric(pv$versions[[versionPosition]]$minor) + 1
		write.ProjectVersion(pv)
	}
	
	return(pv)
}
