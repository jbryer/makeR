\name{getRSSFeed}
\alias{getRSSFeed}
\title{This function ruturns an XML tree of the RSS feed from the given URL.}
\usage{
  getRSSFeed(feedURL, email, passwd, posts = 1000)
}
\arguments{
  \item{feedURL}{the full URL to the RSS feed.}

  \item{email}{the email address for the Google Reader
  account}

  \item{passwd}{the password for the Google Reader account}

  \item{posts}{the number of posts to return}
}
\value{
  the root \code{XMLNode} for the RSS feed.
}
\description{
  This function utilizes the (unofficial) Google Reader API
  to retrieve RSS feeds. The advantage of access RSS feeds
  through the Google Reader API is that you are not limited
  by the number of entries a website may included in their
  feed. That is, Google maintains generally maintains a
  complete history of entries from the RSS feed.
}
\details{
  Note that the contents of the results will be limited by
  what the website provides in their feeds. That is, Google
  does not contain more information per entry then what the
  website originally provided. If the initial feed
  contained only excerpts of the article, the feed from
  Google will too only contain excerpts. Be aware though
  that for sites that do provide the complete contents of
  posts will result in potentially very large downloads.
}
\author{
  Jason Bryer <\email{jason@bryer.org}x>
}
\seealso{
  \code{\link{xmlRoot}} for the format of the returned XML
  tree
}

