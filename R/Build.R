#' Constructor for a Build class. This is an interal method used to create a
#' Build class/object.
#' @param buildXML an XML element.
Build <- function(buildXML=NULL, 
				  buildNum=NULL, major=NULL, minor=NULL, name=NULL, 
				  timestamp=date(), 
				  R=R.version$version.string, 
				  platform=R.version$platform,
				  user=Sys.info()['user'],
				  nodename=Sys.info()['nodename'],
				  file=NULL) {
	build = list()
	if(!is.null(buildXML)) {
		build$build = xmlAttrs(buildXML)[['build']]
		if('major' %in% names(xmlAttrs(buildXML))) {
			build$major = xmlAttrs(buildXML)[['major']]
		} else {
			build$major = NA
		}
		build$minor = xmlAttrs(buildXML)[['minor']]
		if('name' %in% names(xmlAttrs(buildXML))) {
			build$name = xmlAttrs(buildXML)[['name']]
		} else {
			build$name = NA
		}
		build$timestamp = xmlAttrs(buildXML)[['timestamp']]
		build$R = xmlAttrs(buildXML)[['R']]
		build$platform = xmlAttrs(buildXML)[['platform']]
		build$user = xmlAttrs(buildXML)[['user']]
		build$nodename = xmlAttrs(buildXML)[['nodename']]
		build$file = xmlAttrs(buildXML)[['file']] #TODO: Support multiple files
	} else {
		build$build = buildNum
		build$major = major
		build$minor = minor
		build$name = name
		build$timestamp = timestamp
		build$R = R
		build$platform = platform
		build$user = user
		build$nodename = nodename
		build$file = file #TODO: Support multiple files		
	}
	class(build) = 'Build'
	return(build)
}

#' Generic S3 method to print information about a Build class.
#'
#' TODO: Need more documentation 
#'
#' @export
print.Build <- function(x, ...) {
	cat(paste(
		'Build ', x$build, ' for version ', x$major, '.', x$minor, ' (', x$name, ')\n',
		'   Built on ', x$timestamp, ' using ', x$R, '\n',
		'   File built: ', x$file,
		sep=''))
}
