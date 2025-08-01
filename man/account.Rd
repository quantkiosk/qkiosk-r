\name{account}
\alias{qk_account}
\alias{qk_download}
\alias{qk_set_apikey}
\alias{qk_get_apikey}
\title{
QK Account Details
}
\description{
Utilities for account usage.
}
\usage{
qk_account(browser=FALSE, files=FALSE)

qk_download(type = c("data", "univ"), n = 10)

qk_get_apikey(req=TRUE)
qk_set_apikey(apiKey,verbose=TRUE)
}
\arguments{
    \item{browser}{
  open account page in browser
  }
    \item{files}{
  include custom files (if any) in account
  }
    \item{type}{
  data to download - either data or universe files.
  }
    \item{n}{
  number of files to select from
  }
    \item{req}{
  should an error be issued if not present
  }
    \item{apiKey}{
  your QUANTkiosk api key.
  }
    \item{verbose}{
  be verbose
  }
}
\details{
Basic account utility tools to check usage and available credits.
}
\value{
\itemize{
  \item{AsOf}{Time of request} 
  \item{Quota}{Current daily quota in credits}
  \item{Usage}{Current usage in credits consumed}
  \item{DataFilesN}{Number of custom data files available}
  \item{DataFiles}{List of data files}
  \item{UnivFilesN}{Number of custom univ files available}
  \item{UnivFiles}{List of univ files}
}
}
\references{
QUANTkiosk Account Page: \url{https://quantkiosk.com/account}
}
\author{
QUANTkiosk authors.
}
\keyword{ util }
\keyword{ web }
\concept{ QK Account Management }
