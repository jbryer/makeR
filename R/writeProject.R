#' Writes a Project XML file.
#'
#' TODO: Need more documentation 
#'
write.Project <- function(pv) {
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
											 name=b$name,
											 timestamp=b$timestamp,
											 R=b$R,
											 platform=b$platform[[1]],
											 nodename=b$nodename[[1]],
											 user=b$user[[1]],
											 file=b$file))
			builds = addChildren(builds, build)
		}
		root = addChildren(root, builds)
	}
	pv$root = root
	saveXML(pv$root, file=pv$ProjectFile)
	pv$file.info = file.info(pv$ProjectFile)
	return(pv)
}
