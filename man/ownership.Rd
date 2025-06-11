\name{institutional}
\alias{institutional}
\alias{qk_institutional}
\alias{beneficial}
\alias{qk_beneficial}
\alias{activist}
\alias{qk_activist}
\alias{insider}
\alias{qk_insider}
\alias{holders}
\alias{qk_holders}
\encoding{UTF-8}
\title{
QUANTkiosk™ Ownership API Functions
}
\description{
Query all ownership across firms, invidividuals and
issuers.
}
\usage{
qk_institutional(qkid,
                 yyyyqq,
                 qtrs=1,
                 agg = TRUE,
                 wait = 0,
                 quiet = TRUE)

qk_insider(qkid,
           yyyyqq,
           qtrs=1,
           form = c('345', '144'),
           wait = 1,
           quiet = TRUE)

qk_beneficial(qkid,
              yyyyqq,
              qtrs=1,
              form = c('13D13G','13G','13D'),
              wait = 1,
              quiet = TRUE)

qk_activist(qkid,
            yyyyqq,
            qtrs=1,
            form = c('13D','13D13G'),
            wait = 1,
            quiet = TRUE)

qk_holders(qkid,
           yyyyqq,
           qtrs=1,
           wait = 1,
           quiet = TRUE)

}
\arguments{
  \item{qkid}{
the \sQuote{QKID™} of reporting institution or filer.
}
  \item{yyyyqq}{
a number of the form \sQuote{YYYYQQ} where \sQuote{YYYY} is a 4-digit year, and \sQuote{QQ} is
a 2-digit quarter [00,01,02,03,04]. Use in conjuction with \code{qtrs} argument to request additional
prior quarters in one call. Four digit values will be treated as a full year (i.e. \sQuote{YYYY}) and expanded
to YYYY00.
}
  \item{qtrs}{
number of previous  quarters to request (including current \code{yyyyqq}.
}
  \item{form}{
for \code{qk_beneficial} and \code{qk_activist} requests, one of
\sQuote{13D} (activist), \sQuote{13G} (passive), \sQuote{13D13G} (activist + passive). For
\code{qk_insider} requests one of \sQuote{345} (insider) or \sQuote{144} (insider intent).
}
  \item{agg}{
if \code{TRUE}, all other managers reporting are rolled up into one record, otherwise
filing will contain other managers if filed for as distinct holdings for one or more of the same
instruments.
}
  \item{wait}{
time in seconds between calls.
}
  \item{quiet}{
make request silent.
}
}
\details{
QUANTkiosk™ Ownership combines multiple sources of information
in an custom formatted \code{data.frame}. At it's core, the data is used to understand
which participants have control over an entity. These participants file various forms
depending on who they are and what their intent may be.

Ownership reporting periods vary based on regulatory rules. Some are quarterly (e.g. Institutional and Funds),
whereas many are filed based on events (e.g. Insider, Beneficial/Activist). The arrival of the updates
is dependent on filer and SEC rules. All requests are specified for the quarter a filing was made.

To allow for consistent interface, \code{yyyyqq} is specified as the year \sQuote{yyyy} and the 2-digit quarter
being requested. For example, the second quarter of 2023 would be \code{yyyyqq=202302}. To request a full year,
one can use the special quarter \code{00}. To request all years use \code{yyyyqq=0} or \code{yyyyqq=000000}.

All usage is metered by data requested, e.g. four quarters or one year are equal in credits used.

For \sQuote{Institutional} (\code{qk_institutional}) aggregated (\code{agg=TRUE}) requests, we
roll up other managers filed within the main filer's report to better see total positions and
changes. If \code{agg=FALSE}, other managers are displayed as reported with quarter over quarter
change columns reflecting changes in exact reporting lines.
}

\section{Institutional}{
Institutional data consists of holdings reported by asset managers meeting certain criteria. Covering
more than 15,000 managers over the last decade, these are extracted in a point-in-time manner
from forms \sQuote{13F-HR}, \sQuote{13F-NT} and the amended versions of these forms.
}
\section{Insider}{
Insider ownership is reported through forms \sQuote{3}, \sQuote{4}, and \sQuote{5}. These are reports filed in
duplicate for insiders of a company to describe their ownership rights, including acquisition and disposal of
shares. These are filed after a change in ownership.
}
\section{Insider Intent}{
Insider Intent is a newer form, only made public in the last few years. This is used to indicate the intention to buy or sell, \emph{but not the
actual purchase or sale}. These are filed by \sQuote{insiders} of a company using form \sQuote{144}.
}
\section{Holders}{
The holders data includes all owners of a particular company, allowing the cross sectional intitutional holdings in one call. This data is aggregated
from all individual reporting firms by quarter and made available as new filings arrive. At present this only contains \sQuote{13F-HR} reporting, updated
as filings arrive, including amendments to previous positions.
}
\section{Blockholders / Beneficial / Activist}{
Any owner with a position greater than 5\% must file under SEC's \sQuote{Schedule 13G (SC 13G)} or \sQuote{Schedule 13D (SC 13D)}. These are 
used to indicate control at or exceeding this threshold by one or more parties sharing control of the position.
}
\section{Funds}{
The largest of all assets under management comes from mutual and closed-end funds that report on form \sQuote{NPORT-P}. As opposed to
\sQuote{Institutional} filers, these represent publicly traded instruments. Reported quarterly.
}

\value{
A \sQuote{data.frame} of class \code{qkinstitutional} containing holdings or holders. See example data for fields.
}
\references{
SEC's EDGAR® Website: \url{https://www.sec.gov/search-filings}

QK Ownership™: \url{https://quantkiosk.com/ownership}

EDGAR® Form SC 13D, SC 13G FAQ: \url{https://www.sec.gov/divisions/investment/13ffaq}

EDGAR® Form 13F-HR FAQ: \url{https://www.sec.gov/divisions/investment/13ffaq}

EDGAR® Form 3,4,5 FAQ: \url{https://www.sec.gov/divisions/investment/13ffaq}
}
\seealso{
Small filer example: \code{\link{sgcap}}

Large filer example with submanagers: \code{\link{deshaw}}

Holders example: \code{\link{crox}}
}
\author{
QUANTkiosk™
}

\examples{
\dontrun{
citadel <- qk_search_mgr("citadel")
citadel_h<- qk_institutional(citadel, yyyyqq=202301)

# all qtrs of 2023
citadel_h<- qk_institutional(citadel, yyyyqq=202300)

# nvidia's institutional holders for 202501, 202404, 202403, 202402
nvda_h <- qk_holders(qk_ticker("NVDA"), yyyyqq=202501, qtrs=4)

# Restaurant Brands (QSR) block holders for 202501, 202404, 202403, 202402
qsr_b <- qk_beneficial(qk_ticker("QSR"), yyyyqq=2025)
}
}
\keyword{ data }
\concept{ QK Data }
\concept{ QK Institutional Ownership }
