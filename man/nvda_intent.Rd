\name{nvda_intent}
\alias{nvda_intent}
\encoding{UTF-8}
\docType{data}
\title{
Nvidia (NVDA) Restricted or Control Shares Intention to Sell
}
\description{
Corporate insider and affiliates intention to sell disclosure.
}
\usage{nvda_intent}
\format{
  A data frame with 54 observations on the following 24 variables.
  \describe{
    \item{\code{acceptanceDatetime}}{EDGAR® processing time}
    \item{\code{accn}}{internal EDGAR® accession number}
    \item{\code{filedDate}}{date filed in form yyyymmdd}
    \item{\code{filename}}{canonical EDGAR® filing path}
    \item{\code{form}}{filing type: 144 or 144/A}
    \item{\code{issuerCik}}{issuer Central Index Key}
    \item{\code{issuerName}}{issuer name}
    \item{\code{issuerQkid}}{issuer QKID™ symbol}
    \item{\code{filerCik}}{seller Central Index Key}
    \item{\code{filerName}}{seller name}
    \item{\code{relationshipToIssuer}}{a character vector}
    \item{\code{securitiesClassTitle}}{a character vector}
    \item{\code{acquiredDateYYYYMMDD}}{the date to be acquired}
    \item{\code{natureOfAcquisitionTransaction}}{nature of acquisition}
    \item{\code{nameOfPersonfromWhomAcquired}}{entity or person acquired from}
    \item{\code{amountOfSecuritiesAcquired}}{amount of securities acquired}
    \item{\code{paymentDateYYYYMMDD}}{a numeric vector}
    \item{\code{natureOfPayment}}{a character vector}
    \item{\code{price}}{the price at acquisition}
    \item{\code{noOfUnitsSold}}{a numeric vector}
    \item{\code{aggregateMarketValue}}{a numeric vector}
    \item{\code{noOfUnitsOutstanding}}{a numeric vector}
    \item{\code{price2}}{the price of expected sale}
    \item{\code{marketCap}}{market capitalization as reported in USD}
  }
}
\details{
Form 144 is a requirement to disclose the intention to sell restricted shares. It is made up of two different
sections:

\describe{
  \item{Intent}{the proposed amount of shares expected to sell \code{nvda_intent} }
  \item{Sales}{all sales made over the preceeding three months \code{nvda_sales} }
}

The intent section of a form is unique across multiple forms filed by a person or entity. The \dQuote{Sales}
section may contain duplicate entries in the data provided by the API. Thsi is intentional as it represents
the time(s) the information was disclosed.

From the SEC, the securities and people reporting are:

Restricted securities are securities acquired in unregistered, private sales from the issuing company or from an affiliate of the issuer. Investors typically receive restricted securities through private placement offerings, Regulation D offerings, employee stock benefit plans, as compensation for professional services, or in exchange for providing "seed money" or start-up capital to the company. Rule 144(a)(3) identifies what sales produce restricted securities.

Control securities are those held by an affiliate of the issuing company. An affiliate is a person, such as an executive officer, a director or large shareholder, in a relationship of control with the issuer. Control means the power to direct the management and policies of the company in question, whether through the ownership of voting securities, by contract, or otherwise. If you buy securities from a controlling person or "affiliate," you take restricted securities, even if they were not restricted in the affiliate's hands.
}
\references{
Form 144 Investor Information: \url{https://www.sec.gov/about/reports-publications/investorpubsrule144}
}
\examples{
data(nvda_sales)
}
\keyword{datasets}
\concept{Insider Ownership}
\concept{Ownership}
