\name{sgcap}
\alias{sgcap}
\encoding{UTF-8}
\docType{data}
\title{
SG Capital Institutional Ownership Details (Aggregated)
}
\description{
Institutional Ownership API example of small manager filing.
}
\usage{sgcap}
\format{
  A data frame with 76 observations on the following 22 variables.
  \describe{
    \item{\code{filerName}}{name of filer (i.e. fund name)}
    \item{\code{filing}}{canonical EDGAR® filing path}
    \item{\code{submissionType}}{filing type: 13F-HR or 13F-HR/A (amended)}
    \item{\code{reportPeriod}}{period of report}
    \item{\code{filedDate}}{when the data was filed}
    \item{\code{normalizedIssuer}}{official 13f list name of instrument}
    \item{\code{normalizedTitleOfClass}}{official 13f list title or class name of instrument}
    \item{\code{issuerSIC}}{official standardized industrial code}
    \item{\code{issuerSector}}{two-character sector code from QUANTkiosk™}
    \item{\code{issuerTicker}}{common exchange ticker}
    \item{\code{issuerQkid}}{QKID™ symbol}
    \item{\code{value}}{fair market value of position at end of report period}
    \item{\code{shrsOrPrnAmt}}{number of shares held or principal amount}
    \item{\code{putCall}}{one of \sQuote{PUT}, \sQuote{CALL} or empty}
    \item{\code{shrsOrPrnAmtType}}{shares \sQuote{SH} or principal \sQuote{PRN}}
    \item{\code{portWgt}}{portfolio weight (QK™ calculated)}
    \item{\code{hasOtherManager}}{logical value indicating if submanagers are reported}
    \item{\code{QtrsHeld}}{quarters held (QK™ calculated)}
    \item{\code{QOQSshPrnAmt}}{quarter over quarter change in shares or principal amount of holding (QK™ calculated)}
    \item{\code{QOQValue}}{quarter over quarter change in value of holding (QK™ calculated)}
    \item{\code{QOQPortWgt}}{quarter over quarter change in portfolio weight (QK™ calculated)}
    \item{\code{newOrDel}}{is position \sQuote{NEW} or \sQuote{DEL}eted. See details. (QK™ calculated)}
  }
}

\details{
Institutional Ownership consists of quarterly \sQuote{13F-HR} and amended reports \sQuote{13F-HR/A} made by investors who hold at least $100 million of securities
declared by the U.S. Securities and Exchange Commission. Filers include
banks, insurance companies, and hedge funds. The filings are due 45 days after the end of each calendar quarter, and are filed
via the EDGAR® system. The securities reported must be from the official \sQuote{13(f) securities} list published by the SEC. See references section.

QUANTkiosk™ provides access to these filings in two forms at present. The first is an aggregated view
that includes one row per unique instrument held.
The second version (ex. \code{deshaw}) contains additional submanagers details. See the respective help page.

Additional fields not present in the official \sQuote{13F-HR} or \sQuote{13F-HR/A} but provided in both aggregated and non-aggregated views:
\itemize{
  \item issuerSIC
  \item issuerSector
  \item issuerQkid
  \item issuerTicker
}
These fields are useful for tracking changes between quarters as well as intrafiling analysis. 

Note that \code{hasOtherManager} is set to \code{TRUE} or \code{FALSE} if the filing reports other manager holdings. See \code{deshaw} for more details on additional fields in non-aggregated views

Additional fields pre-calculated by QUANTkiosk™ across time:
\itemize{
  \item QOQSshPrnAmt
  \item QOQValue
  \item QOQPortWgt
  \item newOrDel
  \item QtrsHeld
}
QK™ calculates these fields as quarter over quarter change in absolute terms. \code{newOrDel} marks holdings that are \sQuote{NEW} this quarter or \sQuote{DEL}eleted. Deleted entries are
not in the filing, but are provided to identify positions traded out of.  \code{QtrsHeld} shows duration of a continuously held position in aggregate, or in the case of submanager API data,
positions held continuously by the exact combination of submanagers. The latter is less reliable due to reporting considerations in the source data.
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
Non-aggregated (Submanager) example: \code{\link{deshaw}}
}
\examples{
str(sgcap)

\dontrun{

# code that generates SG Capital Ownership data. 
sgcap.cik <- 0001009207
qk_institutional(sgcap.cik, yyyy=2024, qq=1, aggType="aggregated")
}

\dontshow{
stopifnot(all(dim(sgcap) == c(76,22)))
}
}
\keyword{datasets}
\concept{Institutional Ownership}
