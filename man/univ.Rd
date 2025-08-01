\name{qk_univ}
\alias{qk_univ}
\encoding{UTF-8}
\title{
Universe Function
}
\description{
Tools to Fetch Universe Data
}
\usage{
qk_univ(univ, dt, src = "QK", cache=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{univ}{
name of universe
}
  \item{dt}{
date for universe in form yyyymmdd. NOT IMPLEMENTED.
}
  \item{src}{
source of universe - \sQuote{QK} or local file path. NOT IMPLEMENTED.
}
  \item{cache}{
should data come from cache if available
}
}
\details{
The QK universe function allows for named universe requests. Each universe is constructed using a set of
rules.
}
\value{
A \code{qkid} object containing the universe.
}
\references{
\url{https://quantkiosk.com/qkid}
}
\author{
QUANTkiosk™ team.
}
\note{
Neither \code{dt=} or \code{src=} are implemented in this version.
}

\seealso{
See Also \code{\link{qkid}}
}
\examples{
qk100 <- qk_univ("QK100")

# top 1000 widely firms
qk1k <- qk_univ("QK1000")

# next 2000 most widely held firms
qk2k <- qk_univ("QK2000")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory (show via RShowDoc("KEYWORDS")):
\keyword{ data }
\concept{ identifier }
\concept{ symbology }
