\name{account}
\alias{qk_account}
\alias{qk_set_apikey}
\alias{qk_get_apikey}
\title{
QK Account Details
}
\description{
Utilities for account usage.
}
\usage{
qk_account(browser=FALSE)

qk_get_apikey(req=TRUE)
qk_set_apikey(apiKey)
}
\arguments{
    \item{browser}{
  open account page in browser
  }
    \item{req}{
  should an error be issued if not present
  }
    \item{apiKey}{
  your QUANTkiosk api key.
  }
}
\details{
Basic account utility tools to check usage and available credits.
}
\value{
\itemize{
  \item{AsOf}{Time of request} 
  \item{DailyQuota}{Current daily quota in credits}
  \item{DailyUsage}{Current usage in credits consumed}
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
