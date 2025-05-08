\name{qk_audit}
\alias{qk_audit}
\alias{qk_audit.default}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
QUANTkiosk Data Auditing Tools
}
\description{
Methods to audit orginal data sources to verify data correctness and
interpretation.
}
\usage{
qk_audit(x, ...)

\method{qk_audit}{default}(x, row, accn, fpe, open = FALSE, ctx=-1, ...)
}
\arguments{
  \item{x}{
data.frame to audit
}
  \item{row}{
row of data.frame to display source
}
  \item{accn}{
optional accession number (\code{qk_fn})
}
  \item{fpe}{
optional fiscal period end (\code{qk_fn})
}
  \item{open}{
should browser window be opened to orginal source page
}
  \item{ctx}{
limit display of matching data to a smaller context window. defaults to full record.
}
  \item{\dots}{
unused
}
}
\details{
Non-market data is derived from myriad sources and contain billions of datum. To
better prove correctness (and to track data issues), QUANTkiosk provides a
user inteface to interrogate the original sources in a variety of ways.

The first application is for the fundamentals data we curate from billions
of datum published by companies. The \code{qk_audit} interface allows the
researcher to request the source data - \emph{in context} from the original
filings, if applicable.

For most of the standardized financial reporting line items (GAAP), we have
developed item codes to standardize reporting views when possible. These data
are coallesced from multiple places within filings including tabular data and
the \code{xbrl} (extensible business reporting language) mandaded in the US
and in other countries for many instances of reporting.

\code{qk_audit} is designed to be used regularly to inspect context as well
as numerical correctness of data returned by QUANTkiosk's APIs by using
extracted data from the original \code{HTML} filings. These original tables
do not include the additional contextual details present in the \code{qk_fn}
data - often only presenting a row label and a set of period to date values.
We process and extract the additional 30+ fields from a complex internal process
that includes validation steps as well as the very facility to inspect as presented
in \code{qk_audit}.
}
\value{
A displayed version of the text-formatted original html tables, including
context we are assigning and a highlighted section of the html that matches
the data in question.

Some values may we return in the API are actually imputed from the data
that may actually only report data in a period-to-date format.
}
\references{
\url{www.sec.gov}
}
\author{
QUANTkiosk
}
\note{
This functionality is not all inclusive as locating data within a filings is not one for one. Often
data can be presented outside of statements or tables and therefore has no easy to display
data source. This may be possible in future version fo QK.
}

\seealso{
See Also as \code{\link{qk_fn}}
}
\examples{
\dontrun{
jnj <- qk_fn(qk_ticker("JNJ"), "SALE")
jnj |> to_df() |> qk_audit(55)
}
}
\keyword{ api }
\keyword{ data }
\concept{ fundamentals }
\concept{ financials }
\concept{ auditing }
