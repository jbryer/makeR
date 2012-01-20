#' This function ruturns an XML tree of the RSS feed from the given URL.
#'
#' This function utilizes the (unofficial) Google Reader API to retrieve RSS
#' feeds. The advantage of access RSS feeds through the Google Reader API is that
#' you are not limited by the number of entries a website may included in their
#' feed. That is, Google maintains generally maintains a complete history of
#' entries from the RSS feed. 
#' 
#' Note that the contents of the results will be limited by what the website
#' provides in their feeds. That is, Google does not contain more information
#' per entry then what the website originally provided. If the initial feed
#' contained only excerpts of the article, the feed from Google will too only
#' contain excerpts. Be aware though that for sites that do provide the complete
#' contents of posts will result in potentially very large downloads. 
#'
#' @param feedURL the full URL to the RSS feed.
#' @param email the email address for the Google Reader account
#' @param passwd the password for the Google Reader account
#' @param posts the number of posts to return
#' @return the root \code{XMLNode} for the RSS feed.
#' @seealso \code{\link{xmlRoot}} for the format of the returned XML tree
#' @export
#' @author Jason Bryer <\email{jason@@bryer.org}x>
getRSSFeed <- function(feedURL, email, passwd, posts=1000) {
	require(XML)
	require(RCurl)

	#Authenticate with Google
	curlHandle = getCurlHandle(cookiefile="rcookies", ssl.verifyhost=FALSE, ssl.verifypeer=FALSE)
	x = postForm("https://www.google.com/accounts/ClientLogin",
				 accountType="GOOGLE",
				 service="reader",
				 Email=email,
				 Passwd=passwd,
				 source="makeR",
				 curl = curlHandle)
	gtoken = unlist(strsplit(x, "\n"))
	parsed.gtoken <- unlist(strsplit(gtoken[3], "Auth="))
	if (length(parsed.gtoken) >= 2) {
		auth.token <- unlist(strsplit(gtoken[3], "Auth="))[[2]]
	} else {
		stop("Authentication failed.")
	}
	google.auth <- paste("GoogleLogin auth=", auth.token, sep='')
	
	#Function to retrieve up to 1000 posts
	getDoc <- function(n, c=NULL) {
		feedURL = paste("http://www.google.com/reader/atom/feed/", feedURL, "?n=", n, 
						ifelse(is.null(c), "", paste("&c=", c, sep='')),
						sep='')		
		feed = getURL(feedURL, .encoding = 'UTF-8', followlocation=TRUE, 
					  httpheader=c("Authorization"=google.auth),
					  curl=curlHandle)
		doc = xmlTreeParse(feed, asText=TRUE)
		return(xmlRoot(doc))
	}
	
	root = NULL
	continueValue = NULL
	for(i in 1:ceiling(posts / 1000)) {
		r = getDoc(n=ifelse(i == ceiling(posts / 1000), (posts-1) %% 1000 + 1, 1000), 
				   c=continueValue)
		if(is.null(root)) {
			root = r
		} else {
			entries = which(xmlSApply(r, xmlName) == 'entry')
			if(length(entries) > 0) {
				root = addChildren(root, kids=r[entries])
			}
		}
		if(is.null(r[['continuation']])) {
			break #No more posts to retrieve
		} else {
			continueValue = unclass(xmlChildren(r[['continuation']])$text)$value
		}
	}
	return(root)
}
