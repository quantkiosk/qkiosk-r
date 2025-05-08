\name{qk_filter}
\alias{qk_filter}
\alias{qk_sort}
\alias{qk_summary}
\alias{qk_filter.qk_df_institutional}
\alias{qk_sort.qk_df_institutional}
\alias{qk_summary.qk_df_institutional}
\title{
QK Object Manipulation
}
\description{
\sQuote{generic} functions to filter and sort data returned from the QK API. These are helper
functions designed to facilitate exploration of specific data structures provided by the API.
}
\usage{
qk_filter(x, ...)

\method{qk_filter}{qk_df_institutional}(x, by = c("top", "incr", "decr", "new", "del"), ...)

qk_sort(x, ...)
\method{qk_sort}{qk_df_institutional}(x, by = c("value", "shrsOrPrnAmtType", "portWgt", "QtrsHeld", "QOQSshPrnAmt", "QOQValue", "QOQPortWgt"), incr = TRUE, ...)

qk_summary(x, ...)
\method{qk_summary}{qk_df_institutional}(x, n = c(10, 5), ...)
}
\arguments{
  \item{x}{
\R object
}
  \item{by}{
for \code{qk_sort} the column to sort on. For \code{qk_filter}, selection of rows matching description. See Details.
}
  \item{incr}{
sort in numeric increasing order. Set \code{FALSE} to order largest negative values. 
}
  \item{n}{
number of elements in tabular summary.
}
  \item{\dots}{
unused by current methods.
}
}
\details{
\pkg{qkiosk} makes available multiple data sets from the \emph{quantkiosk} platform. These are often information dense and require
further investigation. To help facilitate and align the experience across supported tools (e.g. dashboard, \R, python), a common
set of basic filtering is provided for convenience.

For \sQuote{Institutional Ownership} data returned from \code{qk_institutional}

See \code{\link{deshaw}} and \code{\link{sgcap}} for examples of data. 
}
\value{
filtered and sorted versions of original data.
}
\references{
See \url{https://api.quantkiosk.com/ownership} dashboard examples
}
\author{
QUANTkiosk
}

\seealso{
See Also as \code{\link{qk_institutional}}, ~~~
}
\examples{

\dontrun{
# SG Capital CIK 
sg.cik <- 1510099

# sgcap is available as a dataset in package
sgcap <- qk_institutional(sg.cik, yyyy=2024, qq=1)
}

head(qk_filter(sgcap, by='incr'))
head(qk_sort(sgcap, by='QtrsHeld'))
head(qk_sort(qk_filter(sgcap, by='decr'), by='QOQValue', incr=FALSE))

qk_summary(sgcap)


}
\keyword{ data }
\concept{ summary }
\concept{ QK Ownership }
