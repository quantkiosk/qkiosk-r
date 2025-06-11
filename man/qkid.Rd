\name{qkid}
\alias{qkid}
\alias{.qkidVersion}
\alias{QKID}
\alias{qk_search}
\alias{qk_search_mgr}
\alias{qk_search_co}
\alias{qk_ticker}
\alias{to_ticker}
\alias{qk_cik}
\alias{to_cik}
\alias{qk_permid}
\alias{to_permid}
\alias{qk_figi}
\alias{to_figi}
\alias{qk_isin}
\alias{to_isin}
\alias{qk_cusip}
\alias{to_cusip}
\alias{qk_lei}
\alias{to_lei}
\alias{qk_sector}
\alias{to_sector}
\alias{qk_sic}
\alias{to_sic}
\alias{entity}
\alias{classname}
\alias{instrument}
\alias{cls}
\alias{detail}
\alias{qk_name}
\alias{to_name}
\alias{is_qkid}
\alias{to_qkid}

\alias{is.qkid}
\alias{as.qkid}
\alias{qk.E}
\alias{qk.Entity}
\alias{qk.C}
\alias{qk.Cls}
\alias{qk.I}
\alias{qk.Ticker}
\alias{as.Ticker}
\alias{qk.Figi}
\alias{as.Figi}
\alias{qk.Cik}
\alias{as.Cik}
\alias{qk.Cusip}
\alias{qk.Isin}
\alias{qk.Permid}
\alias{as.Permid}
\alias{qk.LEI}
\alias{qk.Name}
\alias{as.Name}
\encoding{UTF-8}
\title{
QKID™ Tools
}
\description{
Functions to create, extract and convert entity and instrument identifiers
within QUANTkiosk™'s QKID™ open symbology.
}
\usage{
qk_search(x, type=c('issuer','manager','fund','person'), n=10)
qk_search_co(x, n=10)
qk_search_mgr(x, n=10)

qk_ticker(ticker, ...)
to_ticker(qkid, ...)

qk_figi(figi, type = c("figi", "shareClass", "composite"), ...)
to_figi(qkid, type = c("figi", "shareClass", "composite"), ...)

qk_cik(cik, ...)
to_cik(qkid, ...)

qk_permid(permid, type = c("org", "instrument", "quote"), ...)
to_permid(qkid, type = c("org", "instrument", "quote"), ...)

qk_name(name, ...)
to_name(qkid, ...)

qk_sic(sic, ...)
to_sic(qkid, ...)

qk_sector(sector, ...)
to_sector(qkid, ...)

# custom constructor
qkid(qkid, src="qkid", srcid=qkid, retrieved=yyyymmdd())

is.qkid(x)
as.qkid(x)

.qkidVersion(qkid)
classname(qkid)
detail(qkid)

entity(qkid)
cls(qkid)
instrument(qkid)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
object to convert or test, or name to search
}
  \item{qkid}{
\code{qkid} classed object or one that can be converted to.
}
  \item{cik}{
a string representing a CIK (Central Index Key)

}
  \item{permid}{
a string representing a PermID

}
  \item{ticker}{
a string representing a ticker
}
  \item{figi}{
a string representing a FIGI
}
  \item{name}{
a string representing a name
}
  \item{sic}{
Standard Industrial Classification (SIC) code
}
  \item{sector}{
QK™ Sector (see below)
}
  \item{\dots}{
unused
}
  \item{type}{
optional conversion specifics, or one of \sQuote{issuer} or \sQuote{fund} in \code{qk_search_co} or \code{qk_search_mgr}, respectively.
}
  \item{src}{
source used in construction (e.g. \code{qk_ticker("AAPL")} => \sQuote{ticker})
}
  \item{srcid}{
value used in construction (e.g. \code{qk_ticker("AAPL")} => \sQuote{AAPL})
}
  \item{retrieved}{
date of retrieved (or created) qkid
}
  \item{n}{
number of search matches to return
}
}
\details{
\R functions to allow for mapping and manipulation of
QUANTkiosk™'s symbology as well as common identifiers.
}
\value{
QUANTkiosk™'s public identifier, known as \sQuote{QKID™} is the central
identifier used by the QK™ API. It is freely licensed under CC-BY-SA and
is designed to provide institutional quality symbology usable both within
professional asset amanagement as well as for non-professional use.

For a full discussion, please refer to official website below and
the associated documentation. The \sQuote{QKID™} is a portmantaeu identifier,
combining well-established entity and instrument identifiers into a linked
symbology suitable for research and trading. There are three main components
to a fixed-width \sQuote{QKID™}:

\sQuote{QKID™} can also be mapped to sectors and SIC codes.

\itemize{
  \item ENTITY.CLS.INSTRUMENT
    \itemize{
      \item ENTITY: 10 characters uniquely matching an entity
      \item CLS: 4 character code indicating the instrument class
      \item INSTRUMENT: a unique, immutable instrument ID
    }
}

\sQuote{cls} and \sQuote{detail} provide access to \code{qkid} object
internals and details, returning broken-down identifiers or meta details about
the id.

To facilitate discovery, \pkg{qkiosk} provides search functionality. 
\code{qk_search_co} offers fuzzy matching to public companies (e.g. Pfizer or McDonalds),
returning a menu to select the match from. The return value is the matched \sQuote{QKID}.
Similar functionality is available to find investment managers (e.g. D.E Shaw) via
the \code{qk_search_mgr} function.

}
\references{
Official QKID™ site: \url{https://www.quantkiosk.com/qkid}
}
\author{
QUANTkiosk™
}
\note{
Point-in-time QKID™ queries are currently not available outside of a subscription,
this may change in the future.
}

\examples{

# Johnson & Johnson - primary equity
jnj.qkid <- qkid("0000200406.0000.001S5SHQ9")
classname(jnj.qkid)


\dontrun{
qk_search_co("tesla")
qk_search_mgr("pershing")

jnj.qkid <- qk_ticker("JNJ")
to_ticker(jnj.qkid)
to_cik(jnj.qkid)

detail(jnj.qkid)


faang <- qk_ticker(c("META","AAPL","AMZN","NFLX","GOOGL"))
entity(faang)
cls(faang)
instrument(faang)

cls(faang)

to_ticker(faang)
to_name(faang)
to_permid(faang)
to_figi(faang)
to_cik(faang)
}

}
\keyword{ data }
\concept{ QKID }
\concept{ QKID Symbology }
\concept{ QKID Instrument }
\concept{ QKID Entity }
