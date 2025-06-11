.QKID_CIK <- new.env(hash=TRUE)
.cikMapping <- function() {
  if(length(.QKID_CIK)>0) return(TRUE)
  .ciks <- .reqQKID("cik")
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_CIK),.ciks$qkid,.ciks$cik)
  isprimary <- which(!substr(.ciks$qkid,15,15) %in% c("C","P"))
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_CIK),as.character(.ciks$cik[isprimary]),.ciks$qkid[isprimary])
  invisible(TRUE)
}

.QKID_TICKER <- new.env(hash=TRUE)
.tickerMapping <- function() {
  if(length(.QKID_TICKER)>0) return(TRUE)
  .tickers <- .reqQKID("ticker")
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_TICKER),.tickers$qkid,.tickers$ticker)
  isprimary <- which(!substr(.tickers$qkid,15,15) %in% c("C","P"))
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_TICKER),as.character(.tickers$ticker[isprimary]),.tickers$qkid[isprimary])
  invisible(TRUE)
}

.QKID_PERMID <- new.env(hash=TRUE)
.permidMapping <- function() {
  if(length(.QKID_PERMID)>0) return(TRUE)
  .permids <- .reqQKID("permid")
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_PERMID),.permids$qkid,.permids$permid)
  isprimary <- which(!substr(.permids$qkid,15,15) %in% c("C","P"))
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_PERMID),as.character(.permids$permid[isprimary]),.permids$qkid[isprimary])
  invisible(TRUE)
}
.QKID_FIGI <- new.env(hash=TRUE)
.figiMapping <- function() {
  if(length(.QKID_FIGI)>0) return(TRUE)
  .figis <- .reqQKID("figi")
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_FIGI),.figis$qkid,.figis$figi)
  isprimary <- which(!substr(.figis$qkid,15,15) %in% c("C","P"))
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_FIGI),as.character(.figis$figi[isprimary]),.figis$qkid[isprimary])
  invisible(TRUE)
}
.QKID_NAME <- new.env(hash=TRUE)
.nameMapping <- function() {
  if(length(.QKID_NAME)>0) return(TRUE)
  .names <- .reqQKID("name")
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_NAME),.names$qkid,.names$name)
  isprimary <- which(!substr(.names$qkid,15,15) %in% c("C","P"))
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_NAME),as.character(.names$name[isprimary]),.names$qkid[isprimary])
  invisible(TRUE)
}
.QKID_FUNDNAME <- new.env(hash=TRUE)
.fundnameMapping <- function() {
  if(length(.QKID_FUNDNAME)>0) return(TRUE)
  .names <- .reqQKID("fund")
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_FUNDNAME),.names$qkid,.names$fund)
  isprimary <- which(!substr(.names$qkid,15,15) %in% c("C","P"))
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_FUNDNAME),as.character(.names$fund[isprimary]),.names$qkid[isprimary])
  invisible(TRUE)
}

# One to Many
.QKID_SECTOR <- new.env(hash=TRUE)
.sectorMapping <- function() {
  if(length(.QKID_SECTOR)>0) return(TRUE)
  .sectors <- .reqQKID("sector")
  bysector <- split(.sectors, .sectors$sector)
  mapply(function(sector, companies) assign(sector, companies$qkid, envir = .QKID_SECTOR), names(bysector), bysector) 
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_SECTOR),.sectors$qkid,.sectors$sector)
  invisible(TRUE)
}
.QKID_SIC <- new.env(hash=TRUE)
.sicMapping <- function() {
  if(length(.QKID_SIC)>0) return(TRUE)
  .sics <- .reqQKID("sic")
  bysic <- split(.sics, .sics$sic)
  mapply(function(sic, companies) assign(sic, companies$qkid, envir = .QKID_SIC), names(bysic), bysic) 
  mapply(function(id1,id2) assign(id1,id2,envir=.QKID_SIC),.sics$qkid,.sics$sic)
  invisible(TRUE)
}

.reqQKID <- function(id) {
  apiKey <- Sys.getenv("QK_API_KEY")
  if(apiKey == "")
    stop("apiKey not found in QK_API_KEY environment variable. Set in shell or use `Sys.setenv` from R.")

  req <- "https://api.qkiosk.io/data/qkid"
  reqbody <- as.character(toJSON(list(apiKey=apiKey,ids=id),auto_unbox=TRUE))
  
  # using POST
  handle = new_handle()
  handle_setheaders(handle, "Content-Type"="application/json")
  handle_setopt(handle, customrequest = "POST")
  handle_setopt(handle, postfields = reqbody)
  
  resp <- curl_fetch_memory(req, handle)
  if(resp$status_code != "200") {
    warning(paste0("error retrieving ",id,":",resp$status_code), call.=FALSE)
    return(NULL)
  }
  hdrs <- parse_headers(resp$headers, multiple=TRUE)
  res <- fromJSON(rawToChar(resp$content),simplifyVector=FALSE)

  resp <- curl_fetch_memory(res$Urls[[1]])
  read.csv(textConnection(rawToChar(resp$content)), stringsAsFactors=FALSE)
}
