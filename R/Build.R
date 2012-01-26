#' This is an internal method and should not be called directly.
#'
#' Constructor for a Build class. This is an interal method used to create a
#' Build class/object.
#'
#' @param buildXML an XML element.
#' @param buildNum new build number.
#' @param major major version number
#' @param minor minor version number
#' @param name the name of the version
#' @param timestamp the time the file was built
#' @param R the R version information
#' @param platform the platform the file was built
#' @param user the user that performed the build
#' @param nodename the name of the node on which this file was built
#' @param files the name of the files that were built. This is typically a list of PDFs.
Build <- function(buildXML=NULL, 
				  buildNum=NULL, major=NULL, minor=NULL, name=NULL, 
				  timestamp=date(), 
				  R=R.version$version.string, 
				  platform=R.version$platform,
				  user=Sys.info()['user'],
				  nodename=Sys.info()['nodename'],
				  files=NULL) {
	build = list()
	if(!is.null(buildXML)) {
		build$Build = xmlAttrs(buildXML)[['build']]
		if('major' %in% names(xmlAttrs(buildXML))) {
			build$Major = xmlAttrs(buildXML)[['major']]
		} else {
			build$Major = NA
		}
		build$Minor = xmlAttrs(buildXML)[['minor']]
		if('name' %in% names(xmlAttrs(buildXML))) {
			build$Name = xmlAttrs(buildXML)[['name']]
		} else {
			build$Name = NA
		}
		build$Timestamp = xmlAttrs(buildXML)[['timestamp']]
		build$R = xmlAttrs(buildXML)[['R']]
		build$Platform = xmlAttrs(buildXML)[['platform']]
		build$User = xmlAttrs(buildXML)[['user']]
		build$Nodename = xmlAttrs(buildXML)[['nodename']]
		build$Files = character()
		files = which(xmlSApply(buildXML, xmlName) == 'file')
		for(i in files) {
			build$Files = c(build$Files, xmlValue(buildXML[[i]]))
		}
	} else {
		build$Build = buildNum
		build$Major = major
		build$Minor = minor
		build$Name = name
		build$Timestamp = timestamp
		build$R = R
		build$Platform = platform
		build$User = user
		build$Nodename = nodename
		build$Files = files
	}
	class(build) = 'Build'
	return(build)
}

#' Generic S3 method to print information about a Build class.
#'
#' @param x the Build
#' @param ... other non-specified parameters
#' @method print Build
#' @S3method print Build
#' @export
print.Build <- function(x, ...) {
	cat(paste(
		'Build ', x$Build, ' for version ', x$Major, '.', x$Minor, ' (', x$Name, ')\n',
		'   Built on ', x$Timestamp, ' using ', x$R, '\n',
		'   Files built: ', x$Files,
		sep=''))
}
