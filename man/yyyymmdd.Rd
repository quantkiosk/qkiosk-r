\name{yyyymmdd}
\alias{yyyymmdd}
\alias{is.yyyymmdd}
\alias{as.yyyymmdd}
\alias{qtrs}
\alias{days}
\alias{as.POSIXct.yyyymmdd}
\alias{as.POSIXlt.yyyymmdd}
\alias{as.Date.yyyymmdd}
\alias{as.yyyymmdd.Date}
\alias{as.yyyymmdd.numeric}
\alias{today}
\alias{seq.yyyymmdd}
\alias{diff.yyyymmdd}
\title{
Simple Dates
}
\description{
Simplified dates expressed as integers to facilitate
easy API access.
}
\usage{
yyyymmdd(x = Sys.time(), tz = Sys.getenv("TZ"))

today()

qtrs(n)
days(n)

\method{seq}{yyyymmdd}(from, to, by, length.out = NULL, along.with = NULL, ...)

\method{diff}{yyyymmdd}(x, lag = 1, differences = 1, ...)

is.yyyymmdd(x)

as.yyyymmdd(x, ...)

\method{as.yyyymmdd}{Date}(x, ...)
\method{as.yyyymmdd}{numeric}(x, ...)

\method{as.Date}{yyyymmdd}(x, ...)
\method{as.POSIXct}{yyyymmdd}(x, ...)
\method{as.POSIXlt}{yyyymmdd}(x, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
\R object
}
  \item{tz}{
time zone to use internally.
}
  \item{n}{
integer value to offset - may be positive or negative
}
  \item{from}{
starting date. Required
}
  \item{to}{
end date. Optional.
}
  \item{by}{
increment of the sequence. Optional. See \sQuote{Details}.
}
  \item{length.out}{
integer, optional. Desired length of the sequence.
}
  \item{along.with}{
take the length from the length of this argument.
}
  \item{lag}{
an integer indicating which lag to use.
}
  \item{differences}{
an integer indicating the order of the difference.
}
  \item{\dots}{
arguments passed to or from other methdod.
}
}
\details{
\R has a weath of date functionality but lacks a coherent integer
date representation that is capable of date math and proper timezone
handling. This set of tools provides this.
}
\value{
TBD
}
\references{
Most date functionality is provided by conversion to \R's \code{Date} internally and cast
back to a \code{yyyymmdd} before returning. See \code{\link{Date}} and \code{\link{seq.Date}} for more details.
}
\author{
QUANTkiosk
}
\seealso{
See also \code{\link{today}}
}
\examples{

# today - no TZ
today()
is.yyyymmdd(today())

# same output as today() but possible to change TZ
yyyymmdd()

# sequence
seq(yyyymmdd(), yyyymmdd() + 10)

seq(yyyymmdd(), by='month', length.out=10)
seq(yyyymmdd(), by='month', along.with=1:5)

# difference between dates
diff(yyyymmdd()+c(1:5,14:20), lag = 1, differences = 1)

# add durations
yyyymmdd() + 10
yyyymmdd() + qtrs(2)

# subtract durations
yyyymmdd() + qtrs(-2)
yyyymmdd() - qtrs(2)

# convert to yyyymmdd
as.yyyymmdd(Sys.Date())
as.yyyymmdd(20240124)

# convert back to R classes
as.Date(yyyymmdd())
as.POSIXct(yyyymmdd())

}
\keyword{ dates }
\concept{ dates }
