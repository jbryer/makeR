install.packages(c('devtools', 'roxygen2', 'RSQLite', 'ipeds'), 
		repos=c('http://cran.r-project.org', 'http://r-forge.r-project.org'))

require(devtools)
require(roxygen2)
require(RSQLite)
require(ipeds)

setwd("~/Dropbox/Projects") #Mac
setwd("C:/Dropbox/My Dropbox/Projects") #Windows

#Package building
document("irutils", clean=TRUE)
document("irutils")
check_doc("irutils")
build("irutils", binary=FALSE)
build("irutils", binary=TRUE)
install("irutils")
check("irutils")
library(irutils)
ls('package:irutils')

#Build Vignette
setwd("C:/Dropbox/My Dropbox/Projects/irutils")
setwd(paste(getwd(), '/man/doc/', sep=''))
getwd()
Stangle('irutils.Rnw')
Sweave('irutils.Rnw')
texi2dvi('irutils.tex', pdf=TRUE)

#Load included data
data(pisa)


#Setup the SQLite database
data(surveys)
drv = dbDriver("SQLite")
conn = dbConnect(drv, dbname=paste(getwd(), '/irutils/data/ipeds.db', sep=''))
saveIPEDStoDB(conn, surv=surveys$SurveyID, years=2010:2006)
dbListTables(conn)

dbGetQuery(conn, "SELECT SurveyID, Title FROM surveys")

hd = dbReadTable(conn, 'HD')
table(hd$year, useNA='ifany')
dbGetQuery(conn, "SELECT SurveyID, Title FROM surveys")

dbDisconnect(conn)


