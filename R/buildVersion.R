#' This is an internal method and should not be called directly.
#'
#' Builds a new version of a project. See Project$build
#'
#' @param pv the Project
#' @param version.major the version that should be built. This can be either the
#'        the major number or version name.
#' @param saveEnv whether to save build environment as an Rda image file.
#' @param builder the builder function to use.
#' @param clean if TRUE all files in the build directory will be deleted before building.
#' @param sourceFile the name of the source file to build.
#' @param ... other non-specified parameters
buildVersion <- function(pv, version.major=NULL, saveEnv=TRUE, builder=getDefaultBuilder(), 
						 clean=FALSE, sourceFile=pv$SourceFile, ...) {
	buildNum = pv$CurrentBuild + 1
	cv = NULL
	if(is.null(version.major)) {
		cv = pv$Versions[[length(pv$Versions)]]
	} else {
		if(is.numeric(version.major)) {
			cv = pv$Versions[[version.major]]
		} else {
			n = unname(unlist(lapply(pv$Versions, FUN=function(x, ...) { x[['Name']] } )))
			versionPosition = which(n == version.major)
			cv = pv$Versions[[versionPosition]]
			if(is.null(cv)) {
				stop(paste("Could not find version ", version.major, sep=''))
			}
		}
	}
	majorNum = cv$Major
	minorNum = cv$Minor
	name = cv$Name
	
	buildEnv <- new.env() 
	
	cat(paste('Building verison ', ifelse(is.null(name), majorNum, name), '.', 
			  minorNum, '-', buildNum, '...\n', sep=''))
	
	cv$assignProperties(theenv=buildEnv)
	
	if(is.na(name)) {
		buildDir = paste(pv$ProjectDir, '/', pv$BuildDir, '/', majorNum, '.', minorNum, sep='')
	} else {
		buildDir = paste(pv$ProjectDir, '/', pv$BuildDir, '/', name, '.', minorNum, sep='')
	}

	if(clean) {
		unlink(buildDir, force=TRUE, recursive=TRUE)
	}
	
	dir.create(buildDir, recursive=TRUE, showWarnings=FALSE)
	
	srcFiles = list.files(paste(pv$ProjectDir, '/', pv$SourceDir, '/', sep=''))
	for(f in srcFiles) { 
		#Check to make sure the source files have been modified
		src.md5 = md5sum(paste(pv$ProjectDir, '/', pv$SourceDir, '/', f, sep=''))
		dst.md5 = md5sum(paste(buildDir, '/', f, sep=''))
		if(!is.na(dst.md5) & dst.md5 != src.md5) {
			stop(paste('The source file ', f, ' has been modified in the build directory. ',
					   'Specify clean=TRUE to overwrite perform a clean build.', sep=''))
		}
	}
	
	cat('Copying source files...\n')
	file.copy(paste(pv$ProjectDir, '/', pv$SourceDir, '/', srcFiles, sep=''), 
			  to=buildDir, overwrite=TRUE, recursive=TRUE)
	
	wd = eval(setwd(buildDir), envir=buildEnv)
	
	cat(paste('Bulding version ', majorNum, '.', minorNum, '-', buildNum, '\n', sep=''))
	if(is.na(name)) {
		eval(sink(paste('build.', majorNum, '.', minorNum, '-', buildNum, '.log', sep=''), 
			 append=TRUE, split=FALSE), envir=buildEnv)
	} else {
		eval(sink(paste('build.', name, '.', minorNum, '-', buildNum, '.log', sep=''), 
			 append=TRUE, split=FALSE), envir=buildEnv)
	}

	success = FALSE
	filesBuilt = NULL
	try( {
		filesBuilt = builder(pv, buildEnv, ...)
		success = TRUE
	})
	
	sink()
	
	if(saveEnv) {
		rdafile = paste('build.', majorNum, '.', minorNum, '-', buildNum, '.Rda', sep='')
		cat(paste("Saving build environment to ", rdafile, sep=''))
		save(buildEnv, file=rdafile)
	}
	
	eval(setwd(wd), envir=buildEnv)
	
	#Add a build entry
	if(success) {
		assign("CurrentBuild", buildNum, envir=pv)
		b = Build(major=majorNum,
			minor=minorNum,
			buildNum=buildNum,
			name = name,
			#TODO: support multiple files in the Build class
			files=filesBuilt)
		builds = pv$Builds
		builds[[as.character(length(pv$Builds) + 1)]] = b
		assign("Builds", builds, envir=pv)
		
		if(isAutoOpen()) {
			for(i in seq_len(length(filesBuilt))) {
				try(system(paste("open \"", buildDir, "/",
								 filesBuilt[i], "\"", sep="")))
			}
		}
		
		invisible(b)
	} else {
		stop("Build did not complete successfully.")
	}	
}
