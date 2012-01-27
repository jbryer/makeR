## Setup the project directory
wd = setwd(system.file(package='makeR'))
projectDir = paste(getwd(), '/demo/stocksDemo', sep='')
unlink(projectDir, force=TRUE, recursive=TRUE)
dir.create(paste(projectDir, '/source', sep=''), showWarnings=FALSE, recursive=TRUE)
file.copy(paste(getwd(), '/stocks/stocks.R', sep=''), 
		  paste(projectDir, '/source/stocks.R', sep=''))

## Define the custom builder
builder.png <- function(project, theenv, ...) {
	sourceFile = ifelse(is.null(project$SourceFile), '.r$', project$SourceFile)
	wd = eval(getwd(), envir=theenv)
	files = list.files(path=wd, pattern=sourceFile, ignore.case=TRUE)
	for(i in seq_len(length(files))) {
		cat(paste("Executing ", files[i], "...", sep=''))
		sys.source(files[i], envir=theenv)
	}
	return(list.files(path=wd, pattern=".png$", ignore.case=TRUE))
}

## Create the makeR project
stocksProject = Project(name="stocks", projectDir=projectDir, properties=list(
	src = "yahoo", stocks = c("GOOG",'AAPL','AMZN','MSFT')))

## Create a new version for December 2011
stocksProject$newVersion(name='2011-12', properties=list(month='2011-12'))

## Print the project summary
stocksProject

## Build the project using the custom builder
stocksProject$build(builder=builder.png)

## Release the version
stocksProject$release(version='2011-12')

## Clean-up
setwd(wd)
