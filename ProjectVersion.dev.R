install.packages(c('devtools', 'roxygen2', 'RSQLite', 'ipeds'), 
		repos=c('http://cran.r-project.org', 'http://r-forge.r-project.org'))

require(devtools)
require(roxygen2)

setwd("~/Dropbox/Projects") #Mac
setwd("C:/Dropbox/My Dropbox/Projects") #Windows

#Package building
document("ProjectVersion", clean=TRUE)
document("ProjectVersion")
check_doc("ProjectVersion")
build("ProjectVersion", binary=FALSE)
build("ProjectVersion", binary=TRUE)
install("ProjectVersion")
check("ProjectVersion")
library(ProjectVersion)
ls('package:ProjectVersion')

#Build Vignette
setwd("C:/Dropbox/My Dropbox/Projects/ProjectVersion")
setwd(paste(getwd(), '/man/doc/', sep=''))
getwd()
Stangle('ProjectVersion.Rnw')
Sweave('ProjectVersion.Rnw')
texi2dvi('ProjectVersion.tex', pdf=TRUE)


