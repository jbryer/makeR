# makeR 
#### An R package for managing document templates and versions

`makeR` is an R package to help manage R projects (e.g. Sweave reports) where multiple versions are created based upon a single source repository. For example, a monthly report where each versions is identitcal with the exception of easily definable parameters (e.g. date ranges for data extraction, title, etc.). This package is not meant to assist with package development or more complex data analysis projects. For those types of projects, consider [`devtools`](http://github.com/hadley/devtools) or [`ProjectTemplate`](http://projecttemplate.net), respectively.

To install the latest development version using `devtools`, type following in R:

		require(devtools)
		install_github("jbryer/makeR")

The `rbloggers` demo provides a tour of the package.

		require(makeR)
		demo('rbloggers')


### Development
The `makeR` package was developing using the `devtools` package. The following commands provide an easy and efficient approach to package development.

		setwd("~/Project") #Change to your working directory
		document("makeR")
		check_doc("makeR")
		build("makeR")
		install("makeR")
		check("makeR")
		library(makeR)
		ls('package:makeR')
