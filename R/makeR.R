makeR <- function() {
	project <- NULL
	while(TRUE) {
		ans = readline("makeR: ")
		if(tolower(ans) == 'q') {
			return(invisible())
		} else if(tolower(ans) %in% c('h', 'help', '?')) {
			cat(paste("makeR interface commands", 
					  "  q  - quits this interface",
					  "  h  - prints this help message",
					  "  l  - loads a project",
					  "  i  - prints project info",
					  "  nv - creates a new version",
					  sep='\n'))
		} else if(tolower(ans) %in% c('l', 'load')) {
			cat("Please select PROJECT.xml for the project to load...\n")
			f = file.choose()
			project <- Project(projectDir=dirname(f))
		} else if(tolower(ans) %in% c('i', 'info')) {
			print(project)
		} else if(tolower(ans) %in% c('v', 'versions')) {
			print(project$Versions)
		} else if(tolower(ans) %in% c('nv', 'newversion')) {
			
		}
	}
}
