\name{qk_complete}
\alias{qk_complete}
\encoding{UTF-8}
\title{
Live Download EDGAR® Filings Raw & Processed Data
}
\description{
Provide access to QUANTkiosk™ \emph{QK Complete™} professional subscription. This function is intended
to be used as part of an ETL pipeline and run at fixed intervals to provide local live
capture of filings data.
}
\usage{
qk_complete(dir,
           subscription = "raw",
           rollup = "5m",
           last = "5m",
           date = today(),
           fmt = c("zip", "tar.gz"),
           gzip = TRUE,
           hide = TRUE,
           quiet = FALSE,
           force = FALSE)
}
\arguments{
  \item{dir}{
Directory to use for raw filings. Will be created if needed.
}
  \item{subscription}{
Subscription type varies with access. \sQuote{raw} retrieves
all \sQuote{.txt} filings submitted to the EDGAR® system.
}
  \item{rollup}{
Window of time for which new filings are provided.
}
  \item{last}{
Lookback window to query for \code{rollup} archives.
}
  \item{date}{
SEC date to query (America/New_York)
}
  \item{fmt}{
Return archive type. Default is \sQuote{zip} format which contains uncompressed (\sQuote{gzip}) raw
text filings.
}
  \item{gzip}{
Should files be compressed for local storage.
}
  \item{hide}{
Hide API key in request if \code{quiet=TRUE}.
}
  \item{quiet}{
Hide request from output.
}
  \item{force}{
Overwrite files if already downloaded.
}
}
\details{
\code{qk_complete} syncs live data subscriptions in near realtime. For most applications this is sufficient to maintain a full history
locally while the filings are arriving and being processed by the SEC. 
}
\value{
Called for its side-effects, the function returns a list of filings that have been extracted from the archive. The primary result is the
writing of the raw (or processed) filings to disk, as well as associated meta data created by the api.
}
\references{
SEC's EDGAR® Website: \url{https://www.sec.gov/search-filings}
}
\author{
QUANTkiosk™
}
\note{
This endpoint requires both historical and live access. Consult your account page for details.
}

\examples{
\dontrun{
edgardir <- tempdir()
qk_complete(edgardir)

dir(edgardir)
}
}
\keyword{ data }
\concept{ filings }
