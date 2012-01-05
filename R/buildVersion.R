#' Builds a new version of a project.
#'
#' TODO: Need more documentation 
#'
#' @export
buildVersion <- function(pv, version.major=NULL, ...) {
	builds = pv$root[['builds']]
	buildNum = 1
	if(!is.null(builds)) {
		buildNum = as.integer(xmlAttrs(builds)[['current']]) + 1
	} else {
		builds = xmlNode("builds", attrs=c(current=buildNum))
	}
	
	versions = pv$root[['versions']]
	majors = xmlSApply(versions, xmlAttrs)['major',]
	currentVersion = versions[which(majors == max(majors))]
	majorNum = as.integer(xmlAttrs(currentVersion[[1]])[['major']])
	minorNum = as.integer(xmlAttrs(currentVersion[[1]])[['minor']])
	
	cat(paste('Building verison ', majorNum, '.', minorNum, '-', buildNum, '...\n', sep=''))
	
	cat('Setting global properties...\n')
	if(length(pv$properties) > 0) {
		for(i in 1:length(pv$properties)) {
			p = pv$properties[[i]]
			cat(paste(names(pv$properties)[i], ' = ', p[[1]]), '\n', sep='')
			assign(as.character(names(pv$properties)[i]), p[[1]], envir=sys.frame(sys.parent()))
		}
	}
	
	cv = pv$versions[[majorNum]]
	name = cv$name
	cat('Setting version properties...\n')
	if(length(cv$properties) > 0) {
		for(i in 1:length(cv$properties)) {
			p = cv$properties[[i]]
			cat(paste(names(cv$properties)[i], ' = ', p[[1]]), '\n', sep='')
			assign(names(cv$properties)[i], p[[1]], envir=sys.frame(sys.parent()))
		}
	}
	
	if(is.na(name)) {
		buildDir = paste(pv$ProjectDir, '/', pv$buildDir, '/', majorNum, '.', minorNum, '-', buildNum, sep='')
	} else {
		buildDir = paste(pv$ProjectDir, '/', pv$buildDir, '/', name, '.', minorNum, '-', buildNum, sep='')
	}
	dir.create(buildDir, recursive=TRUE, showWarnings=FALSE)
	
	cat('Copying source files...\n')
	file.copy(
		paste(pv$ProjectDir, '/', pv$sourceDir, '/', list.files(paste(pv$ProjectDir, '/', pv$sourceDir, '/', sep='')), sep=''),
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
	cat('Running Stangle...\n')
	Stangle(rnw[1])
	cat('Running Sweave...\n')
	Sweave(rnw[1], debug=TRUE)
	cat('Running texi2dvi...\n')
	texi2dvi(paste(substr(rnw[1], 1, (nchar(rnw[1])-4)), '.tex', sep=''), pdf=TRUE)
	#})
	
	sink()
	
	xmlAttrs(builds)[['current']] = as.character(buildNum)
	
	#Add a build entry
	builds = addChildren(builds, xmlNode("build", attrs=c(
		major=majorNum,
		minor=minorNum,
		build=buildNum,
		name = name,
		timestamp=date(),
		R=R.version$version.string,
		platform=R.version$platform,
		user=Sys.info()['user'],
		nodename=Sys.info()['nodename'],
		file=paste(substr(rnw[1], 1, (nchar(rnw[1])-4)), '.pdf', sep=''))))
	pv$root[['builds']] <- builds
	
	cat('Saving project file...\n')
	saveXML(pv$root, file=pv$ProjectFile)
	setwd(wd)
	pv = ProjectVersion(projectDir=pv$ProjectDir) #TODO: there is a more efficient way of doing this
	return(pv)
}
