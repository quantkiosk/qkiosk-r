\name{nke}
\alias{nke}
\encoding{UTF-8}
\docType{data}
\title{
Nike (NKE) Insider Ownership Data
}
\description{
Insider Ownership API example of large firm.
}
\usage{nke}
\format{
  A data frame with 60 observations on the following 40 variables.
  \describe{
    \item{\code{acceptanceDatetime}}{EDGAR® processing time}
    \item{\code{filing}}{canonical EDGAR® filing path}
    \item{\code{form}}{filing type: 3, 3/A (amended), 4, 4/A or  5, 5/A}
    \item{\code{is10b5One}}{indication if transaction was made pursuant to a contract, instruction or written plan}
    \item{\code{rptPeriod}}{a character vector}
    \item{\code{issuerCik}}{issuer Central Index Key}
    \item{\code{issuerName}}{issuer name}
    \item{\code{issuerTradingSymbol}}{issuer trading symbol}
    \item{\code{issuerQkid}}{issuer QKID™ symbol}
    \item{\code{ownerCik}}{insider Central Index Key}
    \item{\code{ownerName}}{a character vector}
    \item{\code{isDirector}}{is company director}
    \item{\code{isOfficer}}{is company officer}
    \item{\code{officerTitle}}{officer title}
    \item{\code{isTenPct}}{is a ten percent insider}
    \item{\code{secTitle}}{security title}
    \item{\code{secTitleFootnote}}{footnote for security title}
    \item{\code{conversionOrExercisePrice}}{price}
    \item{\code{conversionFootnote}}{a character vector}
    \item{\code{isTxn}}{is reporting line a transaction}
    \item{\code{isDeriv}}{is security a derivative}
    \item{\code{txnDate}}{transaction date}
    \item{\code{execDate}}{a logical vector}
    \item{\code{txnCode}}{transaction code pursuant to Rule 16b-3. See Details.}
    \item{\code{txnDesc}}{transaction footnote}
    \item{\code{txnAmount}}{transaction amount}
    \item{\code{acquiredOrDisposed}}{acquired or disposed}
    \item{\code{txnPrice}}{transaction price}
    \item{\code{txnValue}}{transaction value}
    \item{\code{exerciseDate}}{exercise date}
    \item{\code{exerciseDateFootnote}}{exercise date footnote}
    \item{\code{expirationDate}}{expiration date}
    \item{\code{expirationFootnote}}{expiration footnote}
    \item{\code{underlyingSecTitle}}{underlying security}
    \item{\code{underlyingShs}}{number of shares of underlying}
    \item{\code{postTxnShs}}{shares owned post transaction}
    \item{\code{postTxnFootnote}}{post transaction footnote}
    \item{\code{ownershipForm}}{form of ownership}
    \item{\code{ownershipNature}}{nature of ownership}
    \item{\code{ownershipFootnote}}{ownership footnote}
  }
}
\details{
The \sQuote{Insider Ownership} data provided by QUANTkiosk™ are gathered from SEC Form 3,4 and 5. The forms
are used for initial insider position, changes in position and exit of position, respectively.

These forms must be filed before the end of the second business day following
the day on which a transaction resulting in a change in beneficial ownership has been
executed (see references below).

Codes are as described below (direct from SEC)

\bold{General Transaction Codes}
\describe{
  \item{  P -}{Open market or private purchase of non-derivative or derivative security}
  \item{  S -}{Open market or private sale of non-derivative or derivative security}
  \item{  V -}{Transaction voluntarily reported earlier than required}
}

\bold{Rule 16b-3 Transaction Codes}
\describe{
  \item{  A —}{Grant, award or other acquisition pursuant to Rule 16b-3(d)}
  \item{  D —}{Disposition to the issuer of issuer equity securities pursuant to Rule 16b-3(e)}
  \item{  F —}{Payment of exercise price or tax liability by delivering or withholding securities incident to the receipt, exercise or vesting of a security issued in accordance with Rule 16b-3}
  \item{  I —}{Discretionary transaction in accordance with Rule 16b-3(f) resulting in acquisition or disposition of issuer securities}
  \item{  M —}{Exercise or conversion of derivative security exempted pursuant to Rule 16b-3}
}

\bold{Derivative Securities Codes (Except for transactions exempted pursuant to Rule 16b3)}
\describe{
  \item{  C —}{Conversion of derivative security}
  \item{  E —}{Expiration of short derivative position}
  \item{  H —}{Expiration (or cancellation) of long derivative position with value received}
  \item{  O —}{Exercise of out-of-the-money derivative security}
  \item{  X —}{Exercise of in-the-money or at-the-money derivative security}
}

\bold{Other Section 16(b) Exempt Transaction and Small Acquisition Codes (except for Rule 16b-3 codes above)}
\describe{
  \item{  C —}{Conversion of derivative security}
  \item{  G —}{Bona fide gift}
  \item{  L —}{Small acquisition under Rule 16a-6}
  \item{  W —}{Acquisition or disposition by will or the laws of descent and distribution}
  \item{  Z —}{Deposit into or withdrawal from voting trust}
}

\bold{Other Transaction Codes}
\describe{
  \item{  C —}{Conversion of derivative security}
  \item{  J —}{Other acquisition or disposition (describe transaction)}
  \item{  K —}{Transaction in equity swap or instrument with similar characteristics}
  \item{  U —}{Disposition pursuant to a tender of shares in a change of control transaction}
}
}
\references{
SEC Investor Bulletin: \url{https://www.sec.gov/files/forms-3-4-5.pdf}

Form 4 Instructions: \url{https://www.sec.gov/about/forms/form4data.pdf}
}
\examples{
data(nke)
}
\keyword{datasets}
\concept{Insider Ownership}
\concept{Ownership}
