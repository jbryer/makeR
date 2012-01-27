#' This is an internal method and should not be called directly.
#'
#' Writes a Project XML file.
#'
#' @param pv the Project
write.Project <- function(pv) {
	root = xmlNode("project", attrs=c(name=pv$ProjectName, 
									  buildDir=pv$BuildDir,
									  releaseDir=pv$ReleaseDir,
									  sourceDir=pv$SourceDir,
									  sourceFile=pv$SourceFile))
	for(i in seq_len(length(pv$Properties))) {
		property = xmlNode('property', attrs=c(name=names(pv$Properties[i]),
											   type=class(pv$Properties[[i]])))
		for(p in seq_len(length(pv$Properties[[i]]))) {
			property = addChildren(property, xmlNode("value",pv$Properties[[i]][p]))
		}
		root = addChildren(root, property)
	}
	if(length(pv$Versions) > 0) {
		versions = xmlNode('versions')
		for(i in 1:length(pv$Versions)) {
			version = xmlNode('version', attrs=c(name=pv$Versions[[i]]$Name,
												 major=pv$Versions[[i]]$Major,
												 minor=pv$Versions[[i]]$Minor))
			props = pv$Versions[[i]]$Properties
			for(i in seq_len(length(props))) {
				#version = addChildren(version, xmlNode('property', 
				#									   attrs=c(name=names(props[i]),
				#									   		type=class(props[[i]]))))
				property = xmlNode('property', attrs=c(name=names(props[i]),
													   type=class(props[[i]])))
				for(p in seq_len(length(props[[i]]))) {
					property = addChildren(property, xmlNode("value",props[[i]][p]))
				}
				version = addChildren(version, property)
			}
			versions = addChildren(versions, version)
		}
		root = addChildren(root, versions)
	}
	if(length(pv$Builds) > 0) {
		builds = xmlNode('builds')
		for(i in 1:length(pv$Builds)) {
			b = pv$Builds[[i]]
			build = xmlNode('build', attrs=c(major=b$Major,
											 minor=b$Minor,
											 build=b$Build,
											 name=b$Name,
											 timestamp=b$Timestamp,
											 R=b$R,
											 platform=b$Platform[[1]],
											 nodename=b$Nodename[[1]],
											 user=b$User[[1]] ))
			#Files built
			if(length(b$Files) > 0) {
				for(i in 1:length(b$Files)) {
					build = addChildren(build, xmlNode("file", b$Files[i]))
				}
			}
			builds = addChildren(builds, build)
		}
		root = addChildren(root, builds)
	}
	saveXML(root, file=pv$ProjectFile)
	pv$file.info = file.info(pv$ProjectFile)
	return(pv)
}
