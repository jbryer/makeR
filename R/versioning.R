require(XML)
require(tools)

#TODO: major and minor are not currently used
patchDoc <- function(major=NULL, minor=NULL, build=NULL) {
	
}

ProjectVersion <- function(projectDir=getwd()) {
	pv <- list()
	
	pv$ProjectDir <- projectDir
	pv$ProjectFile <- paste(projectDir, "/PROJECT.xml", sep='')
	
	pv$doc <- xmlTreeParse(pv$ProjectFile, getDTD=FALSE)
	pv$root <- xmlRoot(pv$doc)
	
	pv$properties <- list()
	properties = which(xmlSApply(pv$root, xmlName) == 'property')
	if(length(properties) > 0) {
		for(i in properties) {
			p = pv$root[[i]]
			n = xmlAttrs(p)['name']
			v = xmlAttrs(p)['value']
			t = xmlAttrs(p)['type']
			if(is.na(t)) {
				pv$properties[n] = v
			} else if(t == 'character') {
				pv$properties[n] = as.character(v)
			} else if(t == 'numeric') {
				pv$properties[n] = as.numeric(v)
			} else if(t == 'logical') {
				pv$properties[n] = as.logical(v)
			#} else if(t == 'date') {
			#	pv$properties[n] = as.Date(v)
			} else {
				pv$properties[n] = v
			}
		}
	}
	
	versions = pv$root[['versions']]
	pv$versions = list()
	if(length(versions) > 0) {
		for(ver in 1:length(versions)) {
			v = Version(versions[[ver]])
			pv$versions[[v$major]] = v
		}
	}

	builds = pv$root[['builds']]
	buildNum = 1
	pv$builds = list()
	if(!is.null(builds)) {
		buildNum = as.integer(xmlAttrs(builds)[['current']])
		for(b in 1:length(builds)) {
			build = Build(builds[[b]])
			pv$builds[[build$build]] = build
		}
	}
	pv$CurrentBuild = buildNum
	
	pv$ProjectName = ''
	if('name' %in% names(xmlAttrs(pv$root))) {
		pv$ProjectName = xmlAttrs(pv$root)[['name']]
	} 
	
	if('buildDir' %in% names(xmlAttrs(pv$root))) {
		pv$buildDir = xmlAttrs(pv$root)[['buildDir']]
	} else {
		pv$buildDir = 'build'
	}
	
	if('sourceDir' %in% names(xmlAttrs(pv$root))) {
		pv$sourceDir = xmlAttrs(pv$root)[['sourceDir']]
	} else {
		pv$sourceDir = 'source'
	}
	
	if('releaseDir' %in% names(xmlAttrs(pv$root))) {
		pv$releaseDir = xmlAttrs(pv$root)[['releaseDir']]
	} else {
		pv$releaseDir = 'release'
	}
	
	class(pv) <- "ProjectVersion"
	return(pv)
}

print.ProjectVersion <- function(pv) {
	cat(paste('Project Directory: ', pv$ProjectDir, '\n',
			  'Source Directory: ', pv$sourceDir, '\n',
			  'Build Directory: ', pv$buildDir, '\n',
			  'Current build: ', pv$CurrentBuild, '\n',
			  sep=''))
	if(length(pv$properties) > 0) {
		cat('Properties:\n')
		for(i in 1:length(pv$properties)) {
			p = pv$properties[[i]]
			cat(paste('  ', names(pv$properties)[i], ' = ', p[[1]]), '\n', sep='')
		}
	}
}

write.ProjectVersion <- function(pv) {
	root = xmlNode("project", attrs=c(name=pv$ProjectName, 
					buildDir=pv$buildDir,
					releaseDir=pv$releaseDir,
					sourceDir=pv$sourceDir))
	if(length(pv$properties) > 0) {
		for(i in 1:length(pv$properties)) {
			property = xmlNode('property', attrs=c(name=names(pv$properties[i]),
												   value=pv$properties[[i]],
												   type=class(pv$properties[[i]])))
			root = addChildren(root, property)
		}
	}
	if(length(pv$versions) > 0) {
		versions = xmlNode('versions')
		for(i in 1:length(pv$versions)) {
			version = xmlNode('version', attrs=c(name=pv$versions[[i]]$name,
												 major=pv$versions[[i]]$major,
												 minor=pv$versions[[i]]$minor))
			props = pv$versions[[i]]$properties
			if(length(props) > 0) {
				for(i in 1:length(props)) {
					version = addChildren(version, xmlNode('property', 
										attrs=c(name=names(props[i]),
												value=props[[i]],
												type=class(props[[i]]))))
				}
			}
			versions = addChildren(versions, version)
		}
		root = addChildren(root, versions)
	}
	if(length(pv$builds) > 0) {
		builds = xmlNode('builds')
		for(i in 1:length(pv$builds)) {
			b = pv$builds[[i]]
			build = xmlNode('build', attrs=c(major=b$major,
											 minor=b$minor,
											 build=b$build,
											 timestamp=b$timestamp,
											 R=b$R,
											 platform=b$platform,
											 file=b$file))
			builds = addChildren(builds, build)
		}
		root = addChildren(root, builds)
	}
	pv$root = root
	saveXML(pv$root, file=pv$ProjectFile)	
}

Version <- function(ver) {
	version = list()
	version$major = xmlAttrs(ver)[['major']]
	version$minor = xmlAttrs(ver)[['minor']]
	version$properties = list()
	properties = which(xmlSApply(ver, xmlName) == 'property')
	if(length(properties) > 0) {
		for(i in 1:length(properties)) {
			p = ver[[properties[i]]]
			n = xmlAttrs(p)[['name']]
			v = xmlAttrs(p)[['value']]
			t = xmlAttrs(p)[['type']]
			if(is.na(t)) {
				version$properties[n] = v
			} else if(t == 'character') {
				version$properties[n] = as.character(v)
			} else if(t == 'numeric') {
				version$properties[n] = as.numeric(v)
			} else if(t == 'logical') {
				version$properties[n] = as.logical(v)
			#} else if(t == 'date') {
			#	version$properties[n] = as.Date(v)
			} else {
				version$properties[n] = v
			}
		}
	}
	if('name' %in% names(xmlAttrs(ver))) {
		version$name = xmlAttrs(ver)[['name']]
	} else {
		version$name = NA
	}
	class(version) = 'Version'
	return(version)
}

print.Version <- function(x, ...) {
	cat(paste(
		'Version ', x$major, '.', x$minor, '\nName: ', x$name, '\nProperties:\n', 
		sep=''))
	if(length(x$properties) > 0) {
		for(i in 1:length(x$properties)) {
			p = x$properties[[i]]
			cat(paste('  ', names(x$properties)[i], ' = ', p[[1]]), '\n', sep='')
		}
	}
}

Build <- function(b) {
	build = list()
	build$build = xmlAttrs(b)[['build']]
	if('major' %in% names(xmlAttrs(b))) {
		build$major = xmlAttrs(b)[['major']]
	} else {
		build$major = NA
	}
	build$minor = xmlAttrs(b)[['minor']]
	if('name' %in% names(xmlAttrs(b))) {
		build$name = xmlAttrs(b)[['name']]
	} else {
		build$name = NA
	}
	build$timestamp = xmlAttrs(b)[['timestamp']]
	build$R = xmlAttrs(b)[['R']]
	build$platform = xmlAttrs(b)[['platform']]
	build$file = xmlAttrs(b)[['file']] #TODO: Support multiple files
	class(build) = 'Build'
	return(build)
}

print.Build <- function(x, ...) {
	cat(paste(
		'Build ', x$build, ' for version ', x$major, '.', x$minor, ' (', x$name, ')\n',
		'   Built on ', x$timestamp, ' using ', x$R, '\n',
		'   File built: ', x$file,
		sep=''))
}

buildDoc <- function(pv, version.major=NULL, ...) {
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

releaseDoc <- function(pv, major=NULL, rebuild=FALSE, increment=TRUE, ...) {
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

newVersion <- function(properties=list()) {
	
}


