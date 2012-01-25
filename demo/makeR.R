## makeR demo using R-Bloggers
pause <- function(){  
	invisible(readline("\nPress <return> to continue: ")) 
}

## Google Reader requires you to login to use the API
email = readline("\nEnter your Google Reader email address: ")
passwd = invisible(readline("\nEnter your Google Reader password: "))

## Setup the project directory
wd = setwd(system.file(package='makeR'))
projectDir = paste(getwd(), '/demo/rbloggersDemo', sep='')
unlink(projectDir, force=TRUE, recursive=TRUE)
dir.create(paste(projectDir, '/source', sep=''), showWarnings=FALSE, recursive=TRUE)
file.copy(paste(getwd(), '/rbloggers/rbloggers.Rnw', sep=''), 
		  paste(projectDir, '/source/rbloggers.Rnw', sep=''))
file.copy(paste(getwd(), '/rbloggers/Sweave.sty', sep=''), 
		  paste(projectDir, '/source/Sweave.sty', sep=''))

## Create a new makeR project
myProject = Project(name="RBloggers", projectDir=projectDir, properties=list(
			email=email, passwd=passwd))

myProject$save()

## Create the first version. This will be for summarizing December 2011 posts.
myProject$newVersion(name='2011-12', properties=list(
			startDate='2011-12-01', endDate='2011-12-31'))

## Add Project property
myProject$addProperty("author", "Jason Bryer")

## Print the project summary
myProject

## Can add properties to a specific version
myProject$Versions[[1]]$addProperty("test", "value")

## Build the initial version.
myProject$build()
## Rebuild only the tex file without copying the files over.
myProject$rebuild(builder=builder.tex, sourceFile='rbloggers.tex')
myProject$Builds ## See that the build completed successfully

## Alternatively we can use the cacheSweave builder.
myProject$build(builder=builder.cacheSweave)

## Release the latest version. The version parameter is optional.
myProject$release(version='2011-12')

## Create the second version. This will be for summarizing Januar 2012 posts.
myProject$newVersion(name='2012-01', properties=list(
	startDate='2012-01-01', endDate='2012-01-31'))
myProject

## Build version 2
myProject$build()
myProject$Builds ## See that the build completed successfully

## Release version 2
myProject$release()

## Get the list of released files
myProject$getReleases()

## Remove the project and reload it from the file
rm(myProject)
myProject <- Project(projectDir=projectDir)
myProject

#Rebuild the first version created above
myProject$build(version='2011-12')

## Clean-up
setwd(wd)
