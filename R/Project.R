#' Creates a new empty project.
#'
#' TODO: More documentation
#' 
#' Methods:
#' \tabular{ll}{
#'    build          \tab Builds the project.\cr
#'    rebuild        \tab Rebuilds the project without first copying the files.\cr
#'    save           \tab Saves the PROJECT.xml file.\cr
#'    newVersion     \tab Creates a new versions of the project.\cr
#'    release        \tab Releases a version (i.e. copies the built file to the releases directory)\cr
#'    getProperties  \tab Returns the project properties.\cr
#'    addProperty    \tab Adds a project property.\cr
#'    removeProperty \tab Removes the given project property.\cr
#'    getReleases    \tab Returns a list of released files.\cr
#'    openRelease    \tab Opens the given released file with the system's default application.\cr
#' }
#'
#' @param projectDir the root directory of the project.
#' @param name the name of the project. Only used for new projects.
#' @param sourceDir the directory containing the source files. Only used for new
#'        projects.
#' @param buildDir the directory where built versions will be located. Only used 
#'        for new projects.
#' @param releaseDir the directory where released versions will be located. Only 
#'        used for new projects.
#' @param sourceFile the source file to be built. This can be a file pattern so
#'        that multiple files of the same type can be built.
#' @param properties list of global properties for the project.
#' @export
Project <- function(projectDir=getwd(), name=NULL, sourceDir="source",
					buildDir="build", releaseDir="release", 
					sourceFile=".rnw",
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
	pv$build <- function(version=NULL, saveEnv=TRUE, builder=builder.rnw, ...) { 
		buildVersion(pv, version=version, saveEnv=saveEnv, builder=builder, ...)
		if(isAutoSave()) {
			pv$save()
		}
		invisible()
	}
	pv$rebuild <- function(version=NULL, saveEnv=TRUE, bulder=builder.rnw, ...) {
		buildVersion(pv, version=version, saveEnv=saveEnv, bulder=builder, clean=FALSE, ...)
		if(isAutoSave()) {
			pv$save()
		}
		invisible()
	}
	pv$save <- function() { 
		invisible(write.Project(pv))
	}
	pv$newVersion <- function(name=NA, properties=list()) {
		if(!is.na(name) & name %in% lapply(pv$Versions, FUN=function(x, ...) { x[['Name']] } )) {
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
	pv$release <- function(major=NULL) { 
		releaseVersion(pv, major)
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
		system(paste("open \"", pv$ProjectDir, "/", file, sep=''))
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
	if(length(properties) > 0) {
		for(i in properties) {
			p = root[[i]]
			n = xmlAttrs(p)['name']
			v = xmlAttrs(p)['value']
			t = xmlAttrs(p)['type']
			if(is.na(t)) {
				pv$Properties[[n]] = v
			} else if(t == 'character') {
				pv$Properties[[n]] = as.character(v)
			} else if(t == 'numeric') {
				pv$Properties[[n]] = as.numeric(v)
			} else if(t == 'logical') {
				pv$Properties[[n]] = as.logical(v)
			#} else if(t == 'date') {
			#	pv$Properties[[n]] = as.Date(v)
			} else {
				pv$Properties[[n]] = v
			}
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
#' @param pv the Project
#' @method print Project
#' @S3method print Project
#' @export
print.Project <- function(pv) {
	cat(paste('Project Directory: ', pv$ProjectDir, '\n',
			  'Source Directory: ', pv$SourceDir, '\n',
			  'Build Directory: ', pv$BuildDir, '\n',
			  'Current build: ', pv$CurrentBuild, '\n',
			  sep=''))
	if(length(pv$Properties) > 0) {
		cat('Properties:\n')
		for(i in 1:length(pv$Properties)) {
			p = pv$Properties[[i]]
			cat(paste('  ', names(pv$Properties)[i], ' = ', p[[1]]), '\n', sep='')
		}
	}
	cat(paste('There are currently ', length(pv$Versions), ' versions defined:\n', sep=''))
	print(pv$Versions)
}
