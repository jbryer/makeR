#' Constructor for a Version class. This is an interal method that builds a
#' Version class from XML.
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

#' Creates a new version of the project.
#'
#' TODO: Need more documentation 
#'
#' @export
newVersion <- function(proj, name=NA, properties=list()) {
	if(`_AUTOSAVE`) proj = checkProject(proj)
	version = list()
	version$major = length(proj$versions) + 1
	version$minor = 0
	version$name = name
	version$properties = properties
	class(version) = "Version"
	proj$versions[[as.character(version$major)]] = version
	if(`_AUTOSAVE`) {
		write.Project(proj)
	}
	return(proj)
}

#' Generic S3 method for printing information about a Version class.
#'
#' TODO: Need more documentation 
#'
#' @export
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
