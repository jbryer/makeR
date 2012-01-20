#' This is an internal method and should not be called directly.
#'
#' Builds a new version of a project. See Project$build
#'
#' @param pv the Project
#' @param version.major the version that should be built. This can be either the
#'        the major number or version name.
#' @param ... other non-specified parameters
buildVersion <- function(pv, version.major=NULL, ...) {
	#TODO: Use the major and minor versions parameters to rebuild a specific version
	buildNum = pv$CurrentBuild + 1
	cv = pv$Versions[[length(pv$Versions)]]
	majorNum = cv$Major
	minorNum = cv$Minor
	name = cv$Name
	
	buildEnv <- globalenv() 
	#TODO: Would like to build in a seperate environment, however it appears that
	#      Sweave will only work with the glovalenv().
	
	cat(paste('Building verison ', ifelse(is.null(name), majorNum, name), '.', 
			  minorNum, '-', buildNum, '...\n', sep=''))
	
	cat('Setting global properties...\n')
	if(length(pv$Properties) > 0) {
		for(i in 1:length(pv$Properties)) {
			p = pv$Properties[[i]]
			cat(paste(names(pv$Properties)[i], ' = ', p[[1]]), '\n', sep='')
			assign(as.character(names(pv$Properties)[i]), p[[1]], envir=buildEnv)
		}
	}
	
	cat('Setting version properties...\n')
	if(length(cv$Properties) > 0) {
		for(i in 1:length(cv$Properties)) {
			p = cv$Properties[[i]]
			cat(paste(names(cv$Properties)[i], ' = ', p[[1]]), '\n', sep='')
			assign(names(cv$Properties)[i], p[[1]], envir=buildEnv)
		}
	}
	
	if(is.na(name)) {
		buildDir = paste(pv$ProjectDir, '/', pv$BuildDir, '/', majorNum, '.', minorNum, sep='')
	} else {
		buildDir = paste(pv$ProjectDir, '/', pv$BuildDir, '/', name, '.', minorNum, sep='')
	}
	dir.create(buildDir, recursive=TRUE, showWarnings=FALSE)
	
	cat('Copying source files...\n')
	file.copy(
		paste(pv$ProjectDir, '/', pv$SourceDir, '/', 
			  list.files(paste(pv$ProjectDir, '/', pv$SourceDir, '/', sep='')), sep=''),
		to=buildDir, overwrite=TRUE, recursive=TRUE)
	
	wd = eval(setwd(buildDir), envir=buildEnv)
	
	cat(paste('Bulding version ', majorNum, '.', minorNum, '-', buildNum, '\n', sep=''))
	rnw = list.files(buildDir, pattern=".rnw", ignore.case=TRUE)
	if(is.na(name)) {
		eval(sink(paste('build.', majorNum, '.', minorNum, '-', buildNum, '.log', sep=''), 
			 append=TRUE, split=FALSE), envir=buildEnv)
	} else {
		eval(sink(paste('build.', name, '.', minorNum, '-', buildNum, '.log', sep=''), 
			 append=TRUE, split=FALSE), envir=buildEnv)
	}

	success = FALSE
	try( {
		for(i in 1:length(rnw)) {
			cat('Running Stangle...\n')
			Stangle(rnw[i])
			cat('Running Sweave...\n')
			Sweave(rnw[i], debug=TRUE)
			cat('Running texi2dvi...\n')
			texi2pdf(paste(substr(rnw[i], 1, (nchar(rnw[i])-4)), '.tex', sep=''))
			success = TRUE
		}
	})
	
	sink()
	save(buildEnv, file=paste('build.', majorNum, '.', minorNum, '-', buildNum, '.Rda', sep=''))
	eval(setwd(wd), envir=buildEnv)
	
	#Add a build entry
	if(success) {
		assign("CurrentBuild", buildNum, envir=pv)
		b = Build(major=majorNum,
			minor=minorNum,
			buildNum=buildNum,
			name = name,
			file=paste(substr(rnw[1], 1, (nchar(rnw[1])-4)), '.pdf', sep=''))
		builds = pv$Builds
		builds[[as.character(length(pv$Builds) + 1)]] = b
		assign("Builds", builds, envir=pv)
		
		if(`_AUTOOPEN`) {
			try(system(paste("open \"", buildDir, "/",
					substr(rnw[1], 1, (nchar(rnw[i])-4)), '.pdf', "\"", sep='')))
		}
		
		invisible(b)
	} else {
		stop("Build did not complete successfully.")
	}	
}
