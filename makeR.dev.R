install.packages(c('devtools', 'roxygen2', 'RSQLite', 'ipeds'), 
		repos=c('http://cran.r-project.org', 'http://r-forge.r-project.org'))

require(devtools)
require(roxygen2)

setwd("~/Dropbox/Projects") #Mac
setwd("C:/Dropbox/My Dropbox/Projects") #Windows

#Package building
document("makeR", clean=TRUE)
document("makeR")
check_doc("makeR")
build("makeR", binary=FALSE)
build("makeR", binary=TRUE)
install("makeR")
check("makeR")
library(makeR)
ls('package:makeR')

################################################################################
setwd('~/Dropbox/Projects/TestProject')
pv = ProjectVersion()
pv


