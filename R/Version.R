#' This is an internal method and should not be called directly.
#'
#' Constructor for a Version class. This is an interal method that builds a
#' Version class from XML.
#'
#' @param pv the Project
#' @param name the name of the version
#' @param properties list of version specific properties.
#' @param xml the XML branch if creating from the PROJECT.xml file.
Version <- function(pv, name=NA, properties=list(), xml=NULL) {
	version = list()
	if(!is.null(xml)) { #Parse XML
		version$Major = xmlAttrs(xml)[['major']]
		version$Minor = xmlAttrs(xml)[['minor']]
		version$Properties = list()
		properties = which(xmlSApply(xml, xmlName) == 'property')
		if(length(properties) > 0) {
			for(i in 1:length(properties)) {
				p = xml[[properties[i]]]
				n = xmlAttrs(p)[['name']]
				v = xmlAttrs(p)[['value']]
				t = xmlAttrs(p)[['type']]
				if(is.na(t)) {
					version$Properties[[n]] = v
				} else if(t == 'character') {
					version$Properties[[n]] = as.character(v)
				} else if(t == 'numeric') {
					version$Properties[[n]] = as.numeric(v)
				} else if(t == 'logical') {
					version$Properties[[n]] = as.logical(v)
				#} else if(t == 'date') {
				#	version$Properties[[n]] = as.Date(v)
				} else {
					version$Properties[[n]] = v
				}
			}
		}
		if('name' %in% names(xmlAttrs(xml))) {
			version$Name = xmlAttrs(xml)[['name']]
		} else {
			version$Name = NA
		}
	} else {
		version = list(
			Major = length(pv$Versions) + 1,
			Minor = 0,
			Name = name,
			Properties = properties
		)
		if(isAutoSave()) {
			pv$save()
		}		
	}
	version$pv = pv

	version$getProperties <- function() {
		return(unlist(version$Properites))
	}
	version$addProperty <- function(name, value) { 
		p <- version$Properties
		p[[name]] <- value 
		assign('Properties', p, envir=version)
		assign('Versions', pv$Versions, envir=version$pv)
		if(isAutoSave()) {
			pv$save()
		}
		invisible()
	}
	version$removeProperty <- function(name) {
		p <- version$Properties
		p[[name]] <<- NULL
		assign('Properties', p, envir=version)
		assign('Versions', pv$Versions, envir=version$pv)
		if(isAutoSave()) {
			pv$save()
		}
		invisible()
	}
	
	version <- list2env(version)
	class(version) = 'Version'
	invisible(version)
}

#' Generic S3 method for printing information about a Version class. 
#'
#' @param x the Version
#' @param ... other non-specified parameters
#' @method print Version
#' @S3method print Version
#' @export
print.Version <- function(x, ...) {
	cat(paste(
		'Version ', x$Major, '.', x$Minor, '\nName: ', x$Name, '\nProperties:\n', 
		sep=''))
	if(length(x$Properties) > 0) {
		for(i in 1:length(x$Properties)) {
			p = x$Properties[i]
			cat(paste('  ', names(x$Properties)[i], ' = ', p[[1]]), '\n', sep='')
		}
	}
}
