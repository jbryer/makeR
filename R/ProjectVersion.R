#' Constructor function to create a ProjectVersion project.
#'
#' TODO: Need more documentation 
#'
#' @export
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

#' Generic S3 method to print summary information about a ProjectVersion class.
#'
#' TODO: Need more documentation 
#'
#' @export
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
	cat(paste('There are currently ', length(pv$versions), ' versions defined:\n', sep=''))
	print(pv$versions)
}

#' Writes a ProjectVersion XML file.
#'
#' TODO: Need more documentation 
#'
#' @export
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
