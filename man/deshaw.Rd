\name{deshaw}
\alias{deshaw}
\encoding{UTF-8}
\docType{data}
\title{
D.E. Shaw Institutional Ownership Details (Including Submanagers)
}
\description{
Institutional Ownership API example of large manager filing with submanager details.
}
\usage{deshaw}
\format{
  A data frame with 6481 observations on the following 28 variables.
  \describe{
    \item{\code{filerName}}{name of filer (i.e. fund name)}
    \item{\code{filing}}{canonical EDGAR® filing path}
    \item{\code{submissionType}}{filing type: 13F-HR or 13F-HR/A (amended)}
    \item{\code{reportPeriod}}{period of report}
    \item{\code{filedDate}}{when the data was filed}
    \item{\code{inclMgrs}}{number of submanagers reported for. See \code{otherManager}}
    \item{\code{issuer}}{name of instrument}
    \item{\code{titleOfClass}}{title or class name of instrument}
    \item{\code{issuerSIC}}{official standardized industrial code}
    \item{\code{issuerSector}}{two-character sector code from QUANTkiosk™}
    \item{\code{issuerTicker}}{common exchange ticker}
    \item{\code{issuerQkid}}{QKID™ symbol}
    \item{\code{value}}{fair market value of position at end of report period}
    \item{\code{shrsOrPrnAmt}}{number of shares held or principal amount}
    \item{\code{putCall}}{one of \sQuote{PUT}, \sQuote{CALL} or empty}
    \item{\code{shrsOrPrnAmtType}}{shares \sQuote{SH} or principal \sQuote{PRN}}
    \item{\code{invDiscretion}}{type of discretion over investment decision}
    \item{\code{votingAuthSole}}{number of shares of \sQuote{SOLE} voting authority}
    \item{\code{votingAuthShared}}{number of shares of \sQuote{SHARED} voting authority}
    \item{\code{votingAuthNone}}{number of shares of \sQuote{NONE} voting authority}
    \item{\code{portWgt}}{portfolio weight (QK™ calculated)}
    \item{\code{hasOtherManager}}{logical indicating if record is for other manager}
    \item{\code{otherManager}}{reference number of other manager(s) from filing}
    \item{\code{otherManagerName}}{name of other manager(s) from filing}
    \item{\code{otherManagerFileNumber}}{file number of other manager(s)}
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
that includes one row per unique instrument held. This is described in full in the SG Capital example \code{\link{sgcap}}.
The second version (ex. \code{deshaw}) contains additional submanagers details:
\itemize{
  \item inclMgrs
  \item invDiscretion
  \item votingAuthSole
  \item votingAuthShared
  \item votingAuthNone
  \item otherManager
  \item otherManagerFileNumber
}

Additional fields not present in the official \sQuote{13F-HR} or \sQuote{13F-HR/A} but provided in both aggregated and non-aggregated views:
\itemize{
  \item issuerSIC
  \item issuerSector
  \item issuerQkid
  \item issuerTicker
}
These fields are useful for tracking changes between quarters as well as intrafiling analysis. \code{otherManager} and \code{otherManagerName} provides the reported reference number as filed
for each entry. Details of this requirement are beyond this help page, but reference the official SEC documentation and rules for clarity. QK™ does not merge
submanagers and instead reports the combined submanagers (if reported). For example, a 1;2 would mean the managers 1 and 2 share some aspect of the position.

\code{otherManagerFileNumber} is included to help disambiguate the reported ordering from the actual submanagers reporting. The names for the submanagers can
be had using the \code{meta=TRUE} option in \code{\link{qk_institutional}}.

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

D.E. Shaw Filings: \url{https://www.sec.gov/edgar/browse/?CIK=0001009207}

Form 13F-HR FAQ: \url{https://www.sec.gov/divisions/investment/13ffaq}

Official 13(f) Securities \url{https://www.sec.gov/divisions/investment/13flists}
}
\seealso{
R function: \code{\link{qk_institutional}}
Aggregated example: \code{\link{sgcap}}
}
\examples{
str(deshaw)

\dontrun{

# code that downloads  D.E. Shaw Ownership data. 
deshaw_qkid <- qk_search_mgr("de shaw") # 0001009207.0000.E0000LMPJ
qk_institutional(deshaw_qkid, yyyyqq=202401, agg=FALSE)
}

\dontshow{
stopifnot(all(dim(deshaw) == c(6482,30)))
}
}
\keyword{datasets}
\concept{Institutional Ownership}
