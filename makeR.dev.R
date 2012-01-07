install.packages(c('devtools', 'roxygen2'), repos=c('http://cran.r-project.org'))

require(devtools)
require(roxygen2)

################################################################################
#Package building
setwd("~/Dropbox/Projects") #Mac
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
pv = Project()
pv


