\name{pershing}
\alias{pershing}
\encoding{UTF-8}
\docType{data}
\title{
Pershing Square Beneficial and Activist Details
}
\description{
Beneficial and Activist API example of single manager block positions for 2019-2020
}
\usage{data("pershing")}
\format{
  A data frame with 72 observations on the following 25 variables.
  \describe{
    \item{\code{acceptanceDatetime}}{acceptance time to SEC's EDGAR® system}
    \item{\code{filing}}{canonical EDGAR® filing path}
    \item{\code{form}}{filing type: SC 13D, SC 13G, SC 13D/A or SC 13G/A }
    \item{\code{filedAsOf}}{when the data was filed}
    \item{\code{dateOfEvent}}{date reporting for}
    \item{\code{issuerName}}{name of issuer in submission}
    \item{\code{issuerCik}}{Central Index Key of issuer}
    \item{\code{issuerQkid}}{QKID™ symbol}
    \item{\code{issuerTicker}}{common exchange ticker}
    \item{\code{filerName}}{name of reporting filer}
    \item{\code{filerCik}}{Central Index Key of filer}
    \item{\code{rule13d13g}}{rule being reported for}
    \item{\code{namesOfRptPersons}}{individual or firm name who is reporting}
    \item{\code{isGrp}}{a character vector}
    \item{\code{srcOfFunds}}{a character vector}
    \item{\code{srcOfFundsDesc}}{a character vector}
    \item{\code{placeOfOrg}}{a character vector}
    \item{\code{soleVoting}}{a numeric vector}
    \item{\code{sharedVoting}}{a numeric vector}
    \item{\code{soleDispositoryPwr}}{a numeric vector}
    \item{\code{sharedDispositoryPwr}}{a numeric vector}
    \item{\code{aggAmtOwned}}{a numeric vector}
    \item{\code{pctOfClass}}{a numeric vector}
    \item{\code{personType}}{a character vector}
    \item{\code{personTypeDesc}}{a character vector}
  }
}
\details{
Beneficial and Activist Ownership describe owners (reporting) who maintain a large share of a public company (issuer). The forms
filed when a filer maintains or exceeds a 5\% are \sQuote{SC 13G} and \sQuote{SC 13D}, and the follow-on amendments as thos positions
change.

The difference in form types is related to filer \emph{intent}, specifically whether the 5\% owner is passive, or is
attempting to exert control over the company. The canonical example of an large investor who may file both is \sQuote{Pershing Square},
run by \sQuote{Bill Ackman}. We include this dataset to document the fields. Additional history is available for
all filings of this type as well as for all history.

Through 2024, all filings have come as unstructured text and as such are very difficult to systematically extract data. QUANTkiosk™
runs a complex automated extraction process to provide reliable data in a form that is made available in \R via
\code{qk_beneficial} and \code{qk_activist} functions. 
}
\references{
Ownership homepage: \url{https://www.quantkiosk.com/ownership}

Pershing Square Filings: \url{https://www.sec.gov/edgar/browse/?CIK=0001336528}

Form SC 13D/G information: \url{https://www.sec.gov/rules-regulations/staff-guidance/compliance-disclosure-interpretations/exchange-act-sections-13d-13g-regulation-13d-g-beneficial-ownership-reporting}
}
\examples{
str(pershing)

\dontrun{
pershing.cik <- "0001336528"

qk_beneficial(pershing.cik, yyyy=c(2019,2020), qq=0)
}

}
\keyword{datasets}
\concept{Activist Ownership}
\concept{Beneficial Ownership}
