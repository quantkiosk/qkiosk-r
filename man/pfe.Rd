\name{pfe}
\alias{pfe}
\encoding{UTF-8}
\docType{data}
\title{
Pfizer (PFE) Revenue Data
}
\description{
QUANTkiosk™ Fundamentals API (As Of)
}
\usage{data("pfe")}
\format{
  A data frame with 60 observations on the following 40 variables.
  \describe{
    \item{\code{cik}}{the Central Index Key of the issuer}
    \item{\code{pik}}{the QKID™ Permanent Index Key of the issuer}
    \item{\code{acceptance_time}}{EDGAR® processing time}
    \item{\code{acceptance_utc}}{EDGAR® processing time in unix (seconds since 19700101)}
    \item{\code{stmt}}{statement including the item}
    \item{\code{item}}{the QK™ standardized item code}
    \item{\code{unit}}{unit of measurement - as reported}
    \item{\code{form}}{form filing type: 10-Q, 10-Q/A, 10-K, 10-K/A, 8-K, 8-K/A, 20-F, 20-F/A, 6-K, 6-K/A}
    \item{\code{filename}}{canonical EDGAR® filing path}
    \item{\code{accn}}{EDGAR® accession number}
    \item{\code{fye}}{fiscal year end}
    \item{\code{fyeq}}{fiscal yeat end quarter}
    \item{\code{fpb}}{fiscal period beginning}
    \item{\code{fpe}}{fiscal period end}
    \item{\code{filed}}{date filed}
    \item{\code{fpe2filed}}{days from fiscal period end to filed}
    \item{\code{filed40}}{forty day mark from fpe}
    \item{\code{ffiled}}{first filed - will be different in a restatment}
    \item{\code{cyqtr}}{calendar year quarter available}
    \item{\code{cyear}}{calendar year available}
    \item{\code{cqtr}}{calendar quarter available}
    \item{\code{rptq}}{value - fiscal quarter (reported)} 
    \item{\code{rptqd}}{reporting quarter days}
    \item{\code{rpty}}{value - fiscal year to date (reported)}
    \item{\code{rptyd}}{reporting year days}
    \item{\code{fp}}{fiscal period - e.g. Q1, Q2, Q3, Q4, FY}
    \item{\code{fqtr}}{fiscal quarter - e.g. 1, 2, 3, 4}
    \item{\code{fqd}}{fiscal quarter days}
    \item{\code{fq}}{value - calculated fiscal quarter}
    \item{\code{fytd}}{value - calculated fiscal year to date}
    \item{\code{ttm}}{value - calculated trailing twelve months}
    \item{\code{rstmt}}{restated flag}
    \item{\code{rstmt.q}}{restated flag - quarterly value}
    \item{\code{rstmt.y}}{restated flag - yearly value}
    \item{\code{iq}}{imputed quarterly}
    \item{\code{asof}}{as of date}
    \item{\code{concept_id}}{unique id for auditing}
    \item{\code{fqpy}}{fiscal quarter - prior year}
    \item{\code{fytdpy}}{fiscal year to day - prior year}
    \item{\code{ann}}{annual value - only fye}
  }
}
\details{
QUANTkiosk™ Fundamentals are extracted from original sources, using a systematic process. Data is extracted
and normalized into QK™ concepts that align across time and firms using a set of rules. Most concepts are
currently using \sQuote{US-GAAP} taxonomy. 
}
\source{
QUANTkiosk™ \url{https://www.quantkiosk.com/fundamentals}
}
\examples{
to_df(pfe)
}
\keyword{datasets}
\concept{Fundamental Data}
\concept{Company Financials}
