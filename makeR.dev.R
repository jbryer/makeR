install.packages(c('devtools', 'roxygen2'), repos=c('http://cran.r-project.org'))

require(devtools)
require(roxygen2)
install_github('knitr', 'yihui')

################################################################################
#Package building
setwd("~/Dropbox/Projects") #Mac
document("makeR")
check_doc("makeR")
build("makeR", binary=FALSE)
build("makeR", binary=TRUE)
install("makeR")
check("makeR")
library(makeR)
ls('package:makeR')

?Project

setwd("~/Dropbox/Projects"); detach('package:makeR'); document('makeR'); install('makeR'); library(makeR)

################################################################################
isAutoSave()
setAutoSave(FALSE)
isAutoOpen()
setAutoOpen(FALSE)
getDefaultBuilder()

demo('rbloggers')
demo('stocks')
demo('makeR-knitr')

browseVignettes()
vignette('makeR')
