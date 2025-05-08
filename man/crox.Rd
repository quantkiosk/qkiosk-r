\name{crox}
\alias{crox}
\encoding{UTF-8}
\docType{data}
\title{
Crocs Institutional Holders Details By Issuer
}
\description{
Institutional Holders API example that (tragically) never goes out of style.
}
\usage{crox}
\format{
  A data frame with 214 observations on the following 17 variables.
  \describe{
    \item{\code{issuerName}}{name of company}
    \item{\code{filing}}{canonical EDGAR® filing path}
    \item{\code{submissionType}}{filing type: 13F-HR or 13F-HR/A (amended)}
    \item{\code{reportPeriod}}{period of report}
    \item{\code{filedDate}}{when the data was filed}
    \item{\code{issuerTitleOfClass}}{reported title or class name}
    \item{\code{filerCik}}{Central Index Key of holder}
    \item{\code{filerName}}{name of holder}
    \item{\code{issuerQkid}}{QKID™ symbol}
    \item{\code{value}}{fair market value of position at end of period}
    \item{\code{QOQValue}}{quarter over quarter change in value of holding (QK™ calculated)}
    \item{\code{shrsOrPrnAmt}}{shares \sQuote{SH} or principal \sQuote{PRN}}
    \item{\code{QOQSshPrnAmt}}{quarter over quarter change in shares or principal amount of holding (QK™ calculated)}
    \item{\code{portWgt}}{portfolio weight (QK™ calculated)}
    \item{\code{QOQPortWgt}}{quarter over quarter change in portfolio weight (QK™ calculated)}
    \item{\code{qtrsHeld}}{quarters held (QK™ calculated)}
    \item{\code{newOrDeleted}}{is position \sQuote{NEW} or \sQuote{DEL}eted. See details. (QK™ calculated)}
  }
}
\details{
\code{crox} provides an aggregated view of holdings across all institutional filers. This is a cross-filer view
and is derived from between 5000-10000 individual filings.
}
\references{
Ownership homepage: \url{https://www.quantkiosk.com/ownership}

Ownership Dashboard: \url{https://api.quantkiosk.com/ownership}

Ownership API: \url{https://www.quantkiosk.com/api/ownership}

SG Capital Filings \url{https://www.sec.gov/edgar/browse/?CIK=0001510099}

Form 13F-HR FAQ: \url{https://www.sec.gov/divisions/investment/13ffaq}

Official 13(f) Securities \url{https://www.sec.gov/divisions/investment/13flists}
}
\seealso{
R function: \code{\link{qk_institutional}}
Aggregated Manager example: \code{\link{sgcap}}
Non-aggregated Manager (Submanager) example: \code{\link{deshaw}}
}
\examples{
str(crox)

\dontrun{
qk_holders(qk_ticker("CROX"), yyyy=2024, qq=2)
}
\dontshow{
stopifnot(all(dim(crox) == c(214,17)))
}
}
\keyword{datasets}
\concept{Institutional Ownership}
