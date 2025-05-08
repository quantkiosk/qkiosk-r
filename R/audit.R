qk_audit <- function(x, ...) {
  UseMethod('qk_audit')
}
qk_audit.default <- function(x, row, accn, fpe, open=FALSE, ctx=-1, ...) { 
  if(missing(row) && !is.null(attr(x, "highlight"))) {
    row <- which(attr(x,"highlight"))
  }
  if( missing(row) || length(row) > 1 || row > nrow(x) )
    stop("`row` must specify a single obs of 'x'")

  if(!missing(accn) && !missing(fpe))
    row <- which(x$accn == accn & x$fpe == fpe)

  cik <- x$cik[row]
  accn <- x$accn[row]
  stmt <- x$stmt[row]
  fpb <- x$fpb[row]
  ffiled <- x$ffiled[row]
  filed <- x$filed[row]
  fpe2filed <- x$fpe2filed[row]
  fpe <- x$fpe[row]
  fqd <- x$fqd[row]
  fytd <- x$fytd[row]
  fq <- x$fq[row]
  fytd <- x$fytd[row]
  rpty <- x$rpty[row]
  rptq <- x$rptq[row]
  rptqd <- x$rptqd[row]
  rptyd <- x$rptyd[row]
  fqtr <- x$fqtr[row]
  rstmt<- x$rstmt[row]
  iq<- x$iq[row]
  concept_id <- x$concept_id[row];

  restated <- ""
  if(ffiled < filed) {
    restated <- paste0("\033[48;5;1m\033[1;38;5;15m RESTATED \033[39m\033[49;0m \033[3;38;5;241m(initial filing on ",ffiled,")\033[23;39;0m")
  }
  cat("\n\033[1mFinancial Auditing\033[0m  ",restated,"\n\n")
  cat("  filer:",to_name(qk_cik(cik)),"\n")
  cat(paste0("  cik: ",cik,"  |  accn: ",accn,"  |  statement: ",stmt,"  |  filed: ",filed," (fiscal period to date filed: ",fpe2filed," days)\n"))
  cat(paste0("  fiscal period (",fqtr,"):\n    fpb: ",fpb,"\n    fpe: ",fpe,"\n\n"))
  cat(paste0("  fiscal qtr (",sprintf("% 3s",rptqd)," days):\t",sprintf("% 15s",format(fq  ,big.mark=",")),"\t(",ifelse(iq==1," imputed )",paste0(" reported: ",rptq," )")),"\n"))
  cat(paste0("  fiscal ytd (",sprintf("% 3s",rptyd)," days):\t",sprintf("% 15s",format(fytd,big.mark=",")),"\t( reported: ",rpty," )\n\n"))
  print.qk_df(x[row,rerow=FALSE], rerow=FALSE, maxwidth=min(150, getOption("width")))
  cat("\n")
  a <- .audit_fn(cik, accn, stmt, concept_id, ctx)

  if(isTRUE(open)) {
    browseURL(a$url)
  } else {
    cat(a$stmt,"\n")
    if(restated != "") {
      cat("\n\033[3;38;5;1m           * Restated values are filed in later quarters as either an amended form (e.g. 10-Q/A)\n             or in one of the previous period reference values displayed (or provided in XBRL) filing\033[0m.\n\n") 
    }
  }
  invisible(url)
}

.audit_fn <- function (cik, accn, stmt, concept_id, ctx=-1) {
  apikey <- qk_get_apikey()
  req <- paste0('https://api.qkiosk.io/data/audit?id=',cik,'&req=',accn,'-',stmt,'%2bhash.txt&prd=audit/fn/stmt&apiKey=',apikey)
  resp <- curl_fetch_memory(req)
  if(resp$status_code != "200")
    stop("permission denied - verify your API key is set", call.=FALSE)
  L <- iconv(strsplit(rawToChar(resp$content),"\n")[[1]], "latin1", "ASCII", sub="")
  cids <- substr(L,6,37)
  header <- 1:(which(grepl("cid:",L))[1])
  L <- gsub("^.{38}", "", L)
  m <- match(concept_id,cids)
  L[-m] <- paste0("\033[90m", L[-m], "\033[0m")
  L[ m] <- paste0("\033[34;1m", L[m], "\033[39;0m")
  if( ctx > 0 ) {
    s <- max(1, (m + c(-ctx,ctx))[1])
    e <- min(length(L), (m + c(-ctx,ctx))[2])
    L <- L[ sort(unique(c(header, s:e))) ]
    #L <- L[ sort(unique(c(header,( (m-3):min(m+3,length(L)) )))) ]
    if((m-ctx) > length(header))
      L <- c(L[header],"              ---",L[-header])
  }
  L <- paste(L, collapse = "\n")
  url <- gsub(".*(http.*htm).*","\\1",L[1])
  invisible(list(stmt=L,url=url))
}

