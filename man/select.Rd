\name{qk_select}
\alias{qk_select}
\alias{to_df}
\alias{to_ts}
\alias{full}
\alias{highlight}
\alias{highlight.data.frame}
\alias{as.qk_df}
\alias{to_ts.qk_fn}
\alias{to_ts.qk_df}
\alias{full.qk_fn}
\alias{full.qk_df}
\alias{subset.qk_fn}
\alias{subset.qk_df}
\alias{as.data.frame.qk_fn}
\alias{head.qk_df}
\alias{tail.qk_df}
\title{
Convert and Display QUANTkiosk Objects
}
\description{
Generic functions to extract, transform and display QUANTkiosk api data to common R objects.
}
\usage{
qk_select(x, ...)

to_df(x, ...)
as.qk_df(x, view="", showfull=FALSE, cls="qk_df")

to_ts(x, ...)
\method{to_ts}{qk_fn}(x, i='fq', dt='cyqtr', ...)
\method{to_ts}{qk_df}(x, i='fq', dt='cyqtr', ...)

full(x, showfull=TRUE, ...)
\method{full}{qk_fn}(x, showfull=TRUE, ...)
\method{full}{qk_df}(x, showfull=TRUE, ...)

\method{subset}{qk_fn}(x, ...)
\method{as.data.frame}{qk_fn}(x, ...)

\method{subset}{qk_df}(x, ...)
\method{head}{qk_df}(x, ...)
\method{tail}{qk_df}(x, ...)

highlight(x, ...)
\method{highlight}{data.frame}(x, i, style=list(bg='yellow'),...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
Object to transform. These are dispatched to object specific methods when appropriate.
}
  \item{view}{
internal view represented for use in pit functions
}
  \item{showfull}{
Display all hidden columns.
}
  \item{i}{
Row selection
}
  \item{dt}{
Column to use for indexing by time (to_ts)
}
  \item{style}{
background color for highlighting (not yet implemented)
}
  \item{cls}{
additional classes to add to conversion
}
  \item{\dots}{
Additional arguments depending on object class.
}
}
\details{
Most data fetched from QUANTkiosk results in either a \emph{QK} typed object or a \code{data.frame} of class \code{qk_df}.

Central to manipulation of these data, the package provides a variety of \code{s3} methods to interact with \pkg{base} or \pkg{stats} functionalty.
}

\examples{
head(crox)

highlight(crox, 2:3)
}
\keyword{ manip }
\keyword{ data }
\concept{ formating }
