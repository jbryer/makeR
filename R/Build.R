#' Constructor for a Build class.
#'
#' TODO: Need more documentation 
#'
#' @export
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
