\name{qk_fn}
\alias{qk_fn}
\alias{qk_fncodes}
\alias{qkiditems}
\alias{pit_asof}
\alias{pit_asfiled}
\alias{pitAsOf}
\alias{pitAsFiled}
\alias{qk_select.qk_fn}
\encoding{UTF-8}
\title{
Fundamentals Data
}
\description{
Functions to get \sQuote{as-reported} and \sQuote{revised} fundamentals line item data
from QK™/Financials API.
}
\usage{
qk_fn(qkids,
      items,
      qkid_items = NULL,
      from = 20000101,
      to = today(),
      asof = today(),
      asfiled = FALSE,
      aspit = getOption("qk.aspit",FALSE),
      ticker = TRUE,
      hide = TRUE,
      quiet = TRUE,
      cache = TRUE)

qk_fncodes()

\method{qk_select}{qk_fn}(x, id_item, id, item, i, j, ...)

qkiditems(qkids,items,using=TRUE)

pitAsOf(pit,dt = today(), qtrs=1)

pitAsFiled(pit, dt = today(), qtrs=1)
}
\arguments{
  \item{qkids}{
a vector of \code{qkid} objects using \code{qk_ticker},\code{qk_figi},\code{qk_permid},\code{qk_cik} or \code{qkid}. See \code{is.qkid} for more details.
}
  \item{items}{
a vector or list of vectors containing QUANTkiosk™ item codes (e.g. SALE, NI, AT, ATC) corresponding to US-GAAP, DEI, or other line item(s). See qk_fncodes()
}
  \item{qkid_items}{
an object of class \code{qkid_item} created via \code{qkiditems}. if specified, will ignore \code{qkids} and \code{items}. If \code{qkid_items=NULL} (default) 
\code{qkids} and \code{items} will be converted into a corresponding \code{qkid_object}.
}
  \item{from}{
start date of returned series. 
}
  \item{to}{
end date of returned series. expresses as an integer or \code{yyyymmdd} date object.
}
  \item{asof}{
a number in form of YYYYMMDD. return last known values (incl. restated) for all periods up to \sQuote{asof} date. See details for more information on subscription limits.
}
  \item{asfiled}{
return as-filed values from original report (no restatements).
}
  \item{aspit}{
requests full point-in-time table for historical dates. ONLY AVAILABLE TO ENTERPRISE. By default this is attempted once and if not enabled
either the \emph{latest} point-in-time or as-filed data returned depening on \code{asof} or \code{asfiled} argument values.
}
  \item{ticker}{
should column names use tickers
}
  \item{hide}{
hide API Key from verbose (\code{quiet=FALSE}) output.
}
  \item{quiet}{
control verbosity.
}
  \item{cache}{
defaults to \code{TRUE}
}
  \item{pit}{
\code{qkid-item} timeseries from \code{qk_fn(..., aspit=TRUE)} call.
}
  \item{dt}{
date (yyyymmdd) used for point-in-time calculation. This will be converted using \code{as.yyyymmdd}.
}
  \item{qtrs}{
number of quarters to extract
}
  \item{x}{
an object of \code{qk_fn}
}
  \item{\ldots}{
additional arguments if applicable
}
  \item{id_item}{
a vector of id-items in form \sQuote{TICKER.ITEM} to extract \code{data.frame} from \code{qk_fn} object.
}
  \item{id}{
the ticker of the entity for extraction in \code{qk_select}. See \code{id_item} argument
}
  \item{item}{
a vector or list of vectors containing QUANTkiosk™ item codes (e.g. SALE, NI, AT, ATC) corresponding to US-GAAP, DEI, or other line item(s). See qk_fncodes()
}
  \item{using}{
internal flag to indicate input form.
}
  \item{i}{
extract rows from \code{qk_select} call.
}
  \item{j}{
extract colums from \code{qk_select} call.
}
}
\details{
To view current QUANTkiosk™ item codes call qk_fncodes(). This will return a list
organized by supported statements. This is constantly expanding.

For each item in \code{items} requested a request applied to every qkid in
\code{qkids}. All responses are cached by default. This can be disabled using \code{cache=FALSE}.

For enterprise subscriptions, if \code{view='pit'} is requested the subsequent object may contain restated and
as-filed values and times. Two functions
are provided to resolve to unique observations at each date: \code{pitAsFiled} converts the series to only include the initial
observation recorded, whereas \code{pitAsOf} reconciles restatements up to and including
the \code{date} specified. 
}
\value{
A multivariate \code{xts} timeseries if \code{full=FALSE}, containing one column for each \code{qkid}, \code{item} pair. The column names are derived from the QKID metadata. See examples for more details.

If \code{full=TRUE} a nested-list of data.frames are returned with additional details for each item value. See \code{pfe.sale} sample data. The general object looks like:

\itemize{
  \item qkid (e.g. "0001045810.000.001S5TZJ6" [NVDA])
    \itemize{
      \item sale (xts)
      \item eps (xts)
      \item atc (xts)
    }
  \item qkid (e.g. "0001564408.000.00441QMK5" [SNAP])
    \itemize{
      \item sale (xts)
      \item eps (xts)
      \item atc (xts)
    }
}

}
\references{
SEC EDGAR® filing and reporting rules.
}
\author{
QUANTkiosk™ team.
}
\note{
Depending on a user's subscription level, there may be restrictions on the universe of
available companies and available line items. For \code{asof} queries, the the requests
will resolve to the date of the query unless the subscription is for arbitrary point-in-time
data. See \url{https://quantkiosk.com/point-in-time} for a full discussion of the details.
}

\seealso{
See Also \code{\link{qkid}}
See Also \code{\link{qk_fncodes}}
}
\examples{

\dontrun{
# get list of all QK™ codes
fncodes <- qk_fncodes()

# Pfizer and J&J quarterly revenue (SALE) and net income (NI) 
pfe <- qk_fn(qk_ticker(c("PFE","JNJ")), c("SALE","NI"))

tail( to_df(pfe) ) 
tail( qk_select(pfe, "JNJ.SALE") ) 
# single company, single item - Net Income
qkiditems(qk_ticker("AMD"), "NI")

# single company, multiple items - Revenue + Net Income
qkiditems(qk_ticker("AMD"), c("SALE","NI"))

# multiple companies, single item (used for each companies)
qkiditems(qk_ticker(c("AMD", "AAPL")), "NI")

# multiple companies, multiple items (used for each companies) - Assets + Assets Current
qkiditems(qk_ticker(c("AMD", "AAPL")), c("AT", "ATC"))

# multiple companies, multiple differnt items for each
qkiditems(qk_ticker(c("AMD", "AAPL")), list("NI",c("AT","ATC")))
}
}
\keyword{ data }
\concept{ fundamentals }
\concept{ financials }
\concept{ financial statements }
