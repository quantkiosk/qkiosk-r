\name{account}
\alias{qk_account}
\alias{qk_download}
\alias{qk_set_apikey}
\alias{qk_get_apikey}
\encoding{UTF-8}
\title{
QUANTkiosk™ Account Access
}
\description{
Utilities for account usage and data downloads.
}
\usage{
qk_account(browser=FALSE, files=FALSE)

qk_download(file = NULL,
            type = c("data", "univ", "trials"),
            n = 10, available=FALSE, retjson = TRUE)

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
    \item{file}{
  file to download
  }
    \item{type}{
  data to download - either data or universe files.
  }
    \item{available}{
  return table of files and last modified times
  }
    \item{retjson}{
  should json be returned as raw string
  }
    \item{n}{
  number of files to select from
  }
    \item{req}{
  should an error be issued if not present
  }
    \item{apiKey}{
  your QUANTkiosk™ api key.
  }
    \item{verbose}{
  be verbose
  }
}
\details{
QUANTkiosk™ accounts control access as well as provide for custom data
access. 

To set (or get) your API key the functions \code{qk_get_apikey} and \code{qk_set_apikey}
are provided. These rely your environment variable, which may be set from with
your shell or from your session.

\code{qk_account} provides details on your current usage - as well as access to
any custom data files, universe files, or trial data you may have. To access the
files for download, the \code{qk_download} function allows for browsing your
remote data as well as downloading via an interactive menu or by specifying the name
of the file.
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
QUANTkiosk™ Account Page: \url{https://quantkiosk.com/account}
}
\author{
QUANTkiosk™ authors.
}
\keyword{ util }
\keyword{ web }
\concept{ QK Account Management }
