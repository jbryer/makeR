## This will demonstrate how to use the knitr package with makeR. This will use
## the demo file included with the knitr package.
require(knitr)

## Setup the project directory
exampleFile = system.file("examples", "knitr-minimal.Rnw", package = "knitr")
wd = setwd(system.file(package='makeR'))
projectDir = paste(getwd(), '/demo/knitrDemo', sep='')
unlink(projectDir, force=TRUE, recursive=TRUE)
dir.create(paste(projectDir, '/source', sep=''), showWarnings=FALSE, recursive=TRUE)
file.copy(exampleFile, paste(projectDir, '/source/knitr-minimal.Rnw', sep=''))

## Create a new makeR project
myProject = Project(name="knitr Demo", projectDir=projectDir, sourceFile='knitr-minimal.Rnw')

## Create the first version.
myProject$newVersion(name='knitr Demo 1')

myProject$build(builder=builder.knitr)

