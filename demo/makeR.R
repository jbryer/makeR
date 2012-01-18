## makeR demo using R-Bloggers
pause <- function(){  
	invisible(readline("\nPress <return> to continue: ")) 
}

## Google Reader requires you to login to use the API
email = readline("\nEnter your Google Reader email address: ")
passwd = invisible(readline("\nEnter your Google Reader password: "))

require(makeR)
require(wordcloud)
require(XML)
require(RCurl)

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
myProject = newProject(name="RBloggers", projectDir=projectDir, properties=list(
			email=email, passwd=passwd))

## Create the first version. This will be for summarizing December 2011 posts.
myProject = newVersion(myProject, name='2011-12', properties=list(
			startDate='2011-12-01', endDate='2011-12-31'))

## Print the project summary
myProject

## Build the initial version.
myProject = buildVersion(myProject)
myProject$builds ## See that the build completed successfully

## Release the latest version
myProject = releaseVersion(myProject)

## Clean-up
setwd(wd)
