#' Builds a new version of a project.
#'
#' TODO: Need more documentation 
#'
#' @export
buildVersion <- function(pv, version.major=NULL, version.minor=NULL, ...) {
	#TODO: Use the major and minor versions parameters to rebuild a specific version
	buildNum = pv$CurrentBuild + 1
	cv = pv$versions[[length(pv$versions)]]
	majorNum = cv$major
	minorNum = cv$minor
	name = cv$name
	
	cat(paste('Building verison ', ifelse(is.null(name), majorNum, name), '.', 
			  minorNum, '-', buildNum, '...\n', sep=''))
	
	cat('Setting global properties...\n')
	if(length(pv$properties) > 0) {
		for(i in 1:length(pv$properties)) {
			p = pv$properties[[i]]
			cat(paste(names(pv$properties)[i], ' = ', p[[1]]), '\n', sep='')
			assign(as.character(names(pv$properties)[i]), p[[1]], envir=sys.frame(sys.parent()))
		}
	}
	
	cat('Setting version properties...\n')
	if(length(cv$properties) > 0) {
		for(i in 1:length(cv$properties)) {
			p = cv$properties[[i]]
			cat(paste(names(cv$properties)[i], ' = ', p[[1]]), '\n', sep='')
			assign(names(cv$properties)[i], p[[1]], envir=sys.frame(sys.parent()))
		}
	}
	
	if(is.na(name)) {
		buildDir = paste(pv$ProjectDir, '/', pv$buildDir, '/', majorNum, '.', minorNum, sep='')
	} else {
		buildDir = paste(pv$ProjectDir, '/', pv$buildDir, '/', name, '.', minorNum, sep='')
	}
	dir.create(buildDir, recursive=TRUE, showWarnings=FALSE)
	
	cat('Copying source files...\n')
	file.copy(
		paste(pv$ProjectDir, '/', pv$sourceDir, '/', 
			  list.files(paste(pv$ProjectDir, '/', pv$sourceDir, '/', sep='')), sep=''),
		to=buildDir, overwrite=TRUE, recursive=TRUE)
	
	wd = setwd(buildDir)
	#TODO: Would like the build process to happen in its own environment
	#buildenv = new.env()
	
	cat(paste('Bulding version ', majorNum, '.', minorNum, '-', buildNum, '\n', sep=''))
	rnw = list.files(buildDir, pattern=".rnw", ignore.case=TRUE)
	if(is.na(name)) {
		sink(paste('build.', majorNum, '.', minorNum, '-', buildNum, '.log', sep=''), 
			 append=TRUE, split=FALSE)
	} else {
		sink(paste('build.', name, '.', minorNum, '-', buildNum, '.log', sep=''), 
			 append=TRUE, split=FALSE)
	}
	
	#with(buildenv, {
	for(i in 1:length(rnw)) {
		cat('Running Stangle...\n')
		Stangle(rnw[i])
		cat('Running Sweave...\n')
		Sweave(rnw[i], debug=TRUE)
		cat('Running texi2dvi...\n')
		texi2pdf(paste(substr(rnw[i], 1, (nchar(rnw[i])-4)), '.tex', sep=''))
	}
	#})
	
	sink()
	
	#Add a build entry
	pv$CurrentBuild = buildNum
	b = Build(major=majorNum,
		minor=minorNum,
		buildNum=buildNum,
		name = name,
		file=paste(substr(rnw[1], 1, (nchar(rnw[1])-4)), '.pdf', sep=''))
	pv$builds[[(length(pv$builds) + 1)]] = b
	
	setwd(wd)
	if(`_AUTOSAVE`) {
		write.Project(pv)
	}
	return(pv)
}
