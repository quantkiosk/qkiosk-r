AAPL.FIGI <- "BBG000B9XRY4"

.QKIDCLASSMAP_V4 <- list(`0000` = "Equity", `000C` = "Equity (Call)", `000P` = "Equity (Put)",
    `0010` = "Warrant", `001C` = "Warrant (Call)", `001P` = "Warrant (Put)",
    `0020` = "Unit", `002C` = "Unit (Call)", `002P` = "Unit (Put)",
    `0030` = "Right", `003C` = "Right (Call)", `003P` = "Right (Put)",
    `0300` = "Convert", `0400` = "ADR", `040C` = "ADR (Call)", `040P` = "ADR (Put)",
    `0500` = "Fund", `050C` = "Fund (Call)", `050P` = "Fund (Put)",
    `0510` = "Closed End Fund", `051C` = "Closed End Fund (Call)", `051P` = "Closed End Fund (Put)",
    `0570` = "REIT", `057C` = "REIT (Call)", `057P` = "REIT (Put)",
    `0600` = "Partnership", `060C` = "Partnership (Call)", `060P` = "Partnership (Put)",
    `0210` = "Note", `0220` = "Debt", `0230` = "Debt (MTN)", `0ZZZ` = "Undefined")
.QKIDREGEXP_V4 <- "([0-9]{10})[.]([A-Z0-9]{4})[.]([BCDFGHJKLMNPQRSTVWXYZ0-9]{9})|([0-9]{10})[.]([A-Z0-9]{4})[.](UF[CDFGHJKLMNPQRSTVWXYZ0-9]{6}[A])"

.qkidVersion <- function(qkid) {
  if(is.qkid(qkid))
    qkid <- qkid$qkid

  if(all(grepl(.QKIDREGEXP_V4, qkid))) {
    version <- 4
    attr(version, "rx") <- .QKIDREGEXP_V4
  } else {
    version <- NA
  }
  version
} 

qk_search_person <- function(x,n=10) qk_search(x,type='person',n=n)
qk_search_fund <- function(x,n=10) qk_search(x,type='fund',n=n)

qk_search_mgr <- function(x,n=10) qk_search(x,type='manager',n=n)[menu(qk_search(x,type='manager', n=n)$srcid)]
qk_search_co <- function(x,n=10) qk_search(x,type='issuer',n=n)[menu(qk_search(x,type='issuer', n=n)$srcid)]

qk_search <- function(x, type=c('issuer','manager','fund','person'), n=10) {
  type <- match.arg(type)
  if(type=="manager") {
    .fundnameMapping()
    nm <- grep("^0",names(.QKID_FUNDNAME),invert=TRUE,value=TRUE)
    qk_fundname(nm[order(stringdist(toupper(x), toupper(nm), method='jw', p=.1))[1:n]])
  } else
  if(type=="issuer") {
    .nameMapping()
    nm <- grep("^0",names(.QKID_NAME),invert=TRUE,value=TRUE)
    qk_name(nm[order(stringdist(toupper(x), toupper(nm), method='jw', p=.1))[1:n]])
  } else {
    stop(type,"is currently not supported in this release.")
  }
}

qkid <- function(qkid, src="qkid", srcid=qkid, retrieved=yyyymmdd()) {
  structure(list(qkid=qkid, src=src, srcid=srcid, retrieved=retrieved), class="qkid")
}

is_qkid <- is.qkid <- function(x) inherits(x,"qkid")
to_qkid <- as.qkid <- function(x) {
  if(is_qkid(x))
    return(x)
  
  if(all(grepl(attr(.qkidVersion(x),"rx"),x))) {
    x <- qkid(as.character(x), "qkid", x)
    return(x)
  }
  stop("unable to convert to a valid qkid")
}

length.qkid <- function(x) {
  length(x$qkid)
}
classname <- function (qkid) {
  qkid <- to_qkid(qkid)
  unname(unlist(.QKIDCLASSMAP_V4[cls(qkid)]))
}

# quick extract of components
entity <- function(qkid) {
  e <- substr(qkid$qkid, 0, 10)
  e
}
cls <- function(qkid) {
  cls <- substr(qkid$qkid, 12, 15)
  cls
}
instrument <- function(qkid) {
  i <- substr(qkid$qkid, 17, 25)
  i
}

detail <- function(qkid) {
  UseMethod("detail", qkid)
}
detail.default <- function(qkid) {
  stop("detail requires a qkid compatible object")
}
detail.qkid <- function(qkid) {
  structure(list(Entity=entity(qkid), Class=cls(qkid), Instrument=instrument(qkid)), class="qkid_detail")
}
# print methods
print.qkid_detail <- function(x, ...) {
  cat("QKID:\n")
  cat("  Entity:", x$Entity, "\n")
  cat("  Class:", x$Class, "\n")
  cat("  Instrument:", x$Instrument, "\n")
}


qk_ticker <- function(ticker, ...) {
  if(is.qkid(ticker))
    ticker <- to_ticker(ticker)
  .tickerMapping()
  id <- unclass(unname(unlist(mget(ticker, envir=.QKID_TICKER, ifnotfound=NA))))
  qkid(id, "ticker", ticker)
}
to_ticker <- function(qkid, ...) {
  stopifnot(is_qkid(qkid))
  as.character(qk_ticker(qkid$qkid)$qkid) 
}

qk_cik <- function(cik, ...) {
  if(is_qkid(cik))
    cik <- to_cik(cik)
  cik <- as.character(cik)
  .cikMapping()
  id <- unclass(unname(unlist(mget(cik, envir=.QKID_CIK, ifnotfound=NA))))
  qkid(id, "cik", cik)
}
to_cik <- function(qkid, ...) {
  stopifnot(is_qkid(qkid))
  as.character(qk_cik(qkid$qkid)$qkid) 
}

qk_figi <- function(figi, type=c("figi","shareClass","composite"), ...) {
  .figiMapping()
  if(is_qkid(figi))
    figi <- to_figi(figi)
  id <- unclass(unname(unlist(mget(figi, envir=.QKID_FIGI, ifnotfound=NA))))
  qkid(id, "figi", figi)
}
to_figi <- function(qkid, type=c("figi","shareClass","composite"),  ...) {
  stopifnot(is_qkid(qkid))
  as.character(qk_figi(qkid$qkid)$qkid) 
}

qk_permid <- function(permid, type=c("org", "instrument", "quote"), ... ) {
  if(is_qkid(permid))
    permid <- to_permid(permid)
  permid <- as.character(permid)
  .permidMapping()
  id <- unclass(unname(unlist(mget(permid, envir=.QKID_PERMID, ifnotfound=NA))))
  qkid(id, "permid", permid)
}
to_permid <- function(qkid, type=c("org", "instrument", "quote"), ...) {
  stopifnot(is_qkid(qkid))
  as.character(qk_permid(qkid$qkid)$qkid) 
}

qk_fundname <- function(name, ...) {
  if(is_qkid(name))
    name <- to_name(name)
  name <- as.character(name)
  .fundnameMapping()
  id <- unclass(unname(unlist(mget(name, envir=.QKID_FUNDNAME, ifnotfound=NA))))
  qkid(id, "fundname", name)
}
to_fundname <- function(qkid, ...) {
  stopifnot(is_qkid(qkid))
  as.character(qk_name(qkid$qkid)$qkid) 
}

qk_name <- function(name, ...) {
  if(is_qkid(name))
    name <- to_name(name)
  name <- as.character(name)
  .nameMapping()
  id <- unclass(unname(unlist(mget(name, envir=.QKID_NAME, ifnotfound=NA))))
  qkid(id, "name", name)
}
to_name <- function(qkid, ...) {
  stopifnot(is_qkid(qkid))
  as.character(qk_name(qkid$qkid)$qkid) 
}

# sector
qk_sector <- function(sector, ...) {
  if(is_qkid(sector))
    sector <- to_sector(sector)
  sector <- as.character(sector)
  if(length(sector) > 1)
    stop("qk_sector only accepts a single sector")
  .sectorMapping()
  id <- unclass(unname(unlist(mget(sector, envir=.QKID_SECTOR, ifnotfound=NA))))
  id <- na.omit(id)
  qkid(id, "sector", rep(sector, length=length(id)))
}
to_sector <- function(qkid, ...) {
  stopifnot(is_qkid(qkid))
  as.character(qk_sector(qkid$qkid)$qkid) 
}

# sic
qk_sic <- function(sic, ...) {
  if(is_qkid(sic))
    sic <- to_sic(sic)
  sic <- as.character(sic)
  if(length(sic) > 1)
    stop("qk_sic only accepts a single SIC code")
  .sicMapping()
  id <- unclass(unname(unlist(mget(sic, envir=.QKID_SIC, ifnotfound=NA))))
  id <- na.omit(id)
  qkid(id, "sic", rep(sic, length=length(id)))
}
to_sic <- function(qkid, ...) {
  stopifnot(is_qkid(qkid))
  as.character(qk_sic(qkid$qkid)$qkid) 
}



qk_isin <- function(isin, ...) {
  stop("isin conversion is not yet implemented.")
}
to_isin <- function(x, ...) {
  stop("isin conversion is not yet implemented.")
}
qk_cusip <- function(cusip, ...) {
  stop("cusip conversion is not yet implemented.")
}
to_cusip <- function(qkid, ...) {
  stop("cusip conversion is not yet implemented.")
}
qk_lei <- function(lei, ...) {
  stop("lei conversion is not yet implemented.")
}
to_lei <- function(qkid, ...) {
  stop("lei conversion is not yet implemented.")
}

c.qkid <- function(...) {
  ldots <- list(...)
  qkid(qkid=unlist(lapply(ldots, function(e) e$qkid)), 
       src=unlist(lapply(ldots, function(e) e$src)),
       srcid=unlist(lapply(ldots, function(e) e$srcid))
  )
} 
as.character.qkid <- function(x, ...) x$qkid

`[.qkid` <- function(x, i, ...) {
   qkid(x$qkid[i], src=x$src, srcid=x$srcid[i])
}
`[[.qkid` <- `[.qkid`

print.qkid <- function(x, ...) {
  if(all(x$src=="qkid")) {
    print(x$qkid,quote=FALSE)
  } else {
    print(setNames(x$qkid, nm=x$srcid),quote=FALSE)
  }
}
