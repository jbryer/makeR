#' An R package for managing document versions and templates.
#'
#' The idea of build automation is not new. GNU Make and Java Ant are well established
#' and robust build automation systems but require the use and installation of additional
#' software. The makeR package provides a sim- plified framework written entirely 
#' in R to manage Sweave projects where multiple versions are created based upon
#' a single source repository. For example, a monthly report where each version
#' is identical, with perhaps the exception of easily extracted properties 
#' (e.g. date ranges for data extraction, title, etc.).
#' 
#' Most interaction with the \code{makeR} package will occur through the \code{Project}
#' class. \code{Project} is an S3 class that defines some generic functions
#' (e.g. print). The \code{Project} class contains a number of properties and
#' methods. By convention, properties begin with a capital case letter and methods
#' begin with a lower case letter. Each is accessedd using the dollar sign
#' notation (e.g. \code{myproject$build()}). See below for details for each
#' property and method.
#' 
#' See \code{demo('rbloggers')} for an interactive example.
#'
#' Properties:
#' \itemize{
#'    \item \code{BuildDir} - the directory where builds will occur.
#'    \item \code{Builds} - list of completed builds.
#'    \item \code{CurrentBuild} - an integer of the last build.
#'    \item \code{ProjectDir} - the base directory where the project is located.
#'    \item \code{ProjectFile} - the name of the source file to be built.
#'    \item \code{ProjectName} - the name of the project.
#'    \item \code{Properties} - a list of the project properties. See also \code{p$getProperties()}.
#'    \item \code{ReleaseDir} - the directory where released files will be located.
#'    \item \code{SourceDir} - the directory containing the source files.
#'    \item \code{Versions} - a list of the project versions.
#'    \item \code{file.info} - the file info from the last time the PROJECT.xml file was read.
#' }
#'
#' Methods:
#' \itemize{
#'    \item \code{build} Builds the project.
#'            \code{version} - (optional) the version to build.
#'            \code{saveEnv} - if TRUE, the build environment (.rda) will be saved
#'                           in the build directory.
#'            \code{builder} - the builder function. See also \code{\link{builder.rnw}},
#'                  \code{\link{builder.cacheSweave}}, \code{\link{builder.tex}}
#'            \code{clean} - if TRUE a clean build will be performed.
#'    \item \code{rebuild} Rebuilds the project without first copying the files.
#'            \code{version} - (optional) the version to rebuild.
#'            \code{saveEnv} - if TRUE, the build environment (.rda) will be saved
#'                           in the build directory.
#'            \code{builder} - the builder function.See also \code{\link{builder.rnw}},
#'                  \code{\link{builder.cacheSweave}}, \code{\link{builder.tex}}
#'    \item \code{save} Saves the PROJECT.xml file.
#'    \item \code{newVersion} Creates a new versions of the project.
#'            \code{name} - (optional) the version name.
#'            \code{properties} - version specific properties.
#'    \item \code{release} Releases a version (i.e. copies the built file
#'                            to the releases directory)
#'            \code{version} - (optional) the version to release. If omitted the
#'                           latest version will be released.
#'    \item \code{getProperties} Returns the project properties.
#'    \item \code{addProperty} Adds a project property.
#'            \code{name} - The property name.
#'            \code{value} - The property value.
#'    \item \code{removeProperty} Removes the given project property.
#'            \code{name} - The property name.
#'    \item \code{getReleases} Returns a list of released files.
#'    \item \code{openRelease} Opens the given released file with the system's default application.
#'            \code{file} - The released file to open.
#' }
#'
#' @aliases addProperty build getProperties getReleases newVersion openRelease 
#'          rebuild release removeProperty save
#' @param projectDir the root directory of the project.
#' @param name the name of the project. Only used for new projects.
#' @param sourceDir the directory containing the source files. Only used for new
#'        projects.
#' @param buildDir the directory where built versions will be located. Only used 
#'        for new projects.
#' @param releaseDir the directory where released versions will be located. Only 
#'        used for new projects.
#' @param sourceFile the source file to be built. This can be a file pattern (e.g. .rnw$) so
#'        that multiple files of the same type can be built or a single file. The
#'        comparison will be done ignoring case.
#' @param properties list of global properties for the project.
#' @export
#' @examples
#' \dontrun{
#' myProject = Project(name="RBloggers", projectDir=projectDir, properties=list(email=email, passwd=passwd))
#' myProject$save()
#' ## Create the first version. This will be for summarizing December 2011 posts.
#' myProject$newVersion(name='2011-12', properties=list(startDate='2011-12-01', endDate='2011-12-31'))
#' ## Add Project property
#' myProject$addProperty("author", "Jason Bryer")
#' ## Build the initial version.
#' myProject$build()
#' myProject$rebuild(builder=builder.tex, sourceFile='rbloggers.tex')
#' myProject$Builds ## See that the build completed successfully 
#' ## Release the latest version
#' myProject$release(version='2011-12')
#' }
Project <- function(projectDir=getwd(), name=NULL, sourceDir="source",
					buildDir="build", releaseDir="release", 
					sourceFile=NULL,
					properties=list()) {
	pv <- NULL
	if(file.exists(paste(projectDir, "/PROJECT.xml", sep=''))) {
		pv = parseProjectXML(projectDir=projectDir)
	}  else {
		if(is.null(name)) stop("name is required for a makeR project")
	
		dir.create(projectDir, showWarnings=FALSE, recursive=TRUE)
		pv = list(
			ProjectDir = projectDir,
			ProjectFile = paste(projectDir, "/PROJECT.xml", sep=''),
			CurrentBuild = 0,
			ProjectName = name,
			BuildDir = buildDir,
			SourceDir = sourceDir,
			ReleaseDir = releaseDir,
			Properties = properties,
			SourceFile = sourceFile,
			Versions = list(),
			Builds = list()
		)
		
		dir.create(paste(projectDir, '/', buildDir, sep=''), showWarnings=FALSE, recursive=TRUE)
		dir.create(paste(projectDir, '/', sourceDir, sep=''), showWarnings=FALSE, recursive=TRUE)
		dir.create(paste(projectDir, '/', releaseDir, sep=''), showWarnings=FALSE, recursive=TRUE)
	}

	pv$File.Info <- NULL
	
	#Define methods
	pv$build <- function(version=NULL, saveEnv=TRUE, builder=builder.rnw, sourceFile=pv$SourceFile, ...) { 
		buildVersion(pv, version.major=version, saveEnv=saveEnv, builder=builder, sourceFile=sourceFile,...)
		if(isAutoSave()) {
			pv$save()
		}
		invisible()
	}
	pv$rebuild <- function(version=NULL, saveEnv=TRUE, bulder=builder.rnw, sourceFile=pv$SourceFile, ...) {
		buildVersion(pv, version.major=version, saveEnv=saveEnv, bulder=builder, clean=FALSE, sourceFile=sourceFile, ...)
		if(isAutoSave()) {
			pv$save()
		}
		invisible()
	}
	pv$save <- function() { 
		invisible(write.Project(pv))
	}
	pv$newVersion <- function(name=NA, properties=list()) {
		if(!is.na(name) & name %in% 
			unname(unlist(lapply(pv$Versions, FUN=function(x, ...) { x[['Name']] } ))))
		{
			stop(paste("Project name must be unique (", name, ")", sep=''))
		}
		v = Version(pv, name, properties)
		versions <- pv$Versions
		versions[[as.character(v$Major)]] <- v
		assign("Versions", versions, envir=pv)
		if(isAutoSave()) {
			pv$save()
		}
		return(v)
	}
	pv$getVersion <- function(name) {
		n = unname(unlist(lapply(pv$Versions, FUN=function(x, ...) { x[['Name']] } )))
		return(pv$Versions[[which(n == name)]])
	}
	pv$release <- function(version=NULL) { 
		releaseVersion(pv, version)
		if(isAutoSave()) {
			pv$save()
		}
		invisible()
	}
	pv$getProperties <- function() {
		return(unlist(pv$Properites))
	}
	pv$addProperty <- function(name, value) { 
		p <- pv$Properties
		p[[name]] <- value 
		assign('Properties', p, envir=pv)
		if(isAutoSave()) {
			pv$save()
		}
		invisible()
	}
	pv$removeProperty <- function(name) {
		p <- pv$Properties
		p[[name]] <<- NULL
		assign('Properties', p, envir=pv)
		if(isAutoSave()) {
			pv$save()
		}
		invisible()
	}
	pv$getReleases <- function() {
		list.files(paste(pv$ProjectDir, '/', pv$ReleaseDir, sep=''))
	}
	pv$openRelease <- function(file) {
		system(paste("open \"", pv$ProjectDir, "/", pv$ReleaseDir ,"/", file, sep=''))
	}
	
	pv <- list2env(pv)
	class(pv) <- "Project"
	
	if(isAutoSave()) {
		pv$save()
	}
	
	return(pv)
}

#' This internal method will check to see if the Project class is current with
#' respect to the PROJECT.xml file. If it is out-of-date it will re-read the XML
#' file and return a new Project object.
#'
#' @param pv the Project.
checkProject <- function(pv) {
	finfo = file.info(pv$ProjectFile)
	if(finfo$mtime > pv$file.info$mtime) {
		return(Project(pv$ProjectDir))
	} else {
		return(pv)
	}
}

#' This is an internal method and should not be called directly.
#'
#' This function will parse the XML project file from the given directory.
#' 
#' @param projectDir the directory containing the project file.
#' @param filename the name of the project file.
parseProjectXML <- function(projectDir=getwd(), filename="PROJECT.xml") {
	pv <- list()
	
	pv$ProjectDir <- projectDir
	pv$ProjectFile <- paste(projectDir, "/", filename, sep='')
	pv$File.Info = file.info(pv$ProjectFile)
	
	doc <- xmlTreeParse(pv$ProjectFile, getDTD=FALSE)
	root <- xmlRoot(doc)
	
	pv$Properties <- list()
	properties = which(xmlSApply(root, xmlName) == 'property')
	for(i in seq_len(length(properties))) {
		p = root[[i]]
		n = xmlAttrs(p)['name']
		t = xmlAttrs(p)['type']
		values = which(xmlSApply(p, xmlName) == 'value')
		value = character()
		for(v in seq_len(length(values))) {
			value = c(value, xmlValue(p[[v]]))
		}
		if(is.na(t)) {
			pv$Properties[[n]] = value
		} else if(t == 'character') {
			pv$Properties[[n]] = as.character(value)
		} else if(t == 'numeric') {
			pv$Properties[[n]] = as.numeric(value)
		} else if(t == 'logical') {
			pv$Properties[[n]] = as.logical(value)
		#} else if(t == 'date') {
		#	pv$Properties[[n]] = as.Date(value)
		} else {
			pv$Properties[[n]] = value
		}
	}
	
	versions = root[['versions']]
	pv$Versions = list()
	if(length(versions) > 0) {
		for(ver in 1:length(versions)) {
			v = makeR:::Version(pv, xml=versions[[ver]])
			pv$Versions[[as.character(v$Major)]] = v
		}
	}
	
	builds = root[['builds']]
	pv$Builds = list()
	if(!is.null(builds)) {
		buildNum = as.integer(xmlAttrs(builds)[['current']])
		for(b in 1:length(builds)) {
			build = makeR:::Build(buildXML=builds[[b]])
			pv$Builds[[as.character(build$Build)]] = build
		}
	}
	pv$CurrentBuild = length(pv$Builds)
	
	pv$SourceFile = NULL
	if('sourceFile' %in% names(xmlAttrs(root))) {
		pv$SourceFile = xmlAttrs(root)[['sourceFile']]
	}
	pv$ProjectName = ''
	if('name' %in% names(xmlAttrs(root))) {
		pv$ProjectName = xmlAttrs(root)[['name']]
	} 
	
	if('buildDir' %in% names(xmlAttrs(root))) {
		pv$BuildDir = xmlAttrs(root)[['buildDir']]
	} else {
		pv$BuildDir = 'build'
	}
	
	if('sourceDir' %in% names(xmlAttrs(root))) {
		pv$SourceDir = xmlAttrs(root)[['sourceDir']]
	} else {
		pv$SourceDir = 'source'
	}
	
	if('releaseDir' %in% names(xmlAttrs(root))) {
		pv$ReleaseDir = xmlAttrs(root)[['releaseDir']]
	} else {
		pv$ReleaseDir = 'release'
	}
	
	return(pv)
}

#' Generic S3 method to print summary information about a Project class.
#'
#' @param x the Project
#' @param ... other unspecified parameters.
#' @method print Project
#' @S3method print Project
#' @export
print.Project <- function(x, ...) {
	cat(paste('Project Directory: ', x$ProjectDir, '\n',
			  'Source Directory: ', x$SourceDir, '\n',
			  'Build Directory: ', x$BuildDir, '\n',
			  'Current build: ', x$CurrentBuild, '\n',
			  sep=''))
	if(length(x$Properties) > 0) {
		cat('Properties:\n')
		for(i in 1:length(x$Properties)) {
			p = x$Properties[[i]]
			cat(paste('  ', names(x$Properties)[i], ' = ', paste(p, collapse=', '), '\n', sep=''))
		}
	}
	cat(paste('There are currently ', length(x$Versions), ' versions defined:\n', sep=''))
	print(x$Versions)
}
