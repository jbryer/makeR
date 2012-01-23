#' This is an internal method and should not be called directly.
#'
#' Builds a new version of a project. See Project$build
#'
#' @param pv the Project
#' @param version.major the version that should be built. This can be either the
#'        the major number or version name.
#' @param saveEnv whether to save build environment as an Rda image file.
#' @param builder the builder function to use.
#' @param clean if TRUE the source files will be (re)copied to the buld directory.
#' @param sourceFile the name of the source file to build.
#' @param ... other non-specified parameters
buildVersion <- function(pv, version.major=NULL, saveEnv=TRUE, builder=builder.rnw, 
						 clean=TRUE, sourceFile=pv$SourceFile, ...) {
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
	
	if(clean) {
		cat('Copying source files...\n')
		file.copy(
			paste(pv$ProjectDir, '/', pv$SourceDir, '/', 
				  list.files(paste(pv$ProjectDir, '/', pv$SourceDir, '/', sep='')), sep=''),
			to=buildDir, overwrite=TRUE, recursive=TRUE)
	}
	
	wd = eval(setwd(buildDir), envir=buildEnv)
	
	cat(paste('Bulding version ', majorNum, '.', minorNum, '-', buildNum, '\n', sep=''))
	rnw = list.files(buildDir, pattern=sourceFile, ignore.case=TRUE)
	if(is.na(name)) {
		eval(sink(paste('build.', majorNum, '.', minorNum, '-', buildNum, '.log', sep=''), 
			 append=TRUE, split=FALSE), envir=buildEnv)
	} else {
		eval(sink(paste('build.', name, '.', minorNum, '-', buildNum, '.log', sep=''), 
			 append=TRUE, split=FALSE), envir=buildEnv)
	}

	success = FALSE
	fileBuilt = NULL
	try( {
		for(i in 1:length(rnw)) {
			fileBuilt = builder(rnw[i], buildEnv, ...)
		}
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
			file=fileBuilt)
		builds = pv$Builds
		builds[[as.character(length(pv$Builds) + 1)]] = b
		assign("Builds", builds, envir=pv)
		
		if(isAutoOpen()) {
			try(system(paste("open \"", buildDir, "/",	fileBuilt, "\"", sep="")))
		}
		
		invisible(b)
	} else {
		stop("Build did not complete successfully.")
	}	
}
