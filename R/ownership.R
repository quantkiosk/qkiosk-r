qk_fund <- function() { stop("not yet implemented") }

qk_activist <- function(qkid, yyyyqq, qtrs=1, form=c("13D","13D13G"), wait=1, quiet=TRUE) {
  check_qkid(qkid)
  form <- match.arg(form)
  qk_beneficial(qkid,yyyyqq,qtrs,form=form,quiet=quiet,wait=wait)
}

qk_beneficial <- function(qkid, yyyyqq, qtrs=1, form=c("13D13G","13G","13D"), wait=1, quiet=TRUE) {
  check_qkid(qkid)
  apiKey <- Sys.getenv("QK_API_KEY")
  if(apiKey == "") 
    stop("apiKey not found in QK_API_KEY environment variable. Set in shell or use `Sys.setenv` from R.")

  by <- "filing"
  ep <- "ownership"

  retType <- "csv"

  form <- match.arg(form)
  #form <- switch(form, D="13D", G="13G", DG="13D13G", stop("invalid form type"))

  to_from <- qtrsback(yyyyqq,qtrs)
  yyyy <- sprintf("%04d",to_from %/% 100)
  yyyy <- unique(yyyy)
  qq <- rep(sprintf("%02d",0),length.out=length(yyyy))

  id <- entity(qkid)
  ticker <- to_ticker(qkid)

  vsn <- NA
  reqs <- 1
  totreqs <- length(qq)
  x <- lapply(1:totreqs, function(i) {
    cat(sprintf("fetching %s (%s) for %s%s ...",ticker,id,yyyy[i],qq[i]))
    req <- sprintf("https://api.qkiosk.io/data/ownership?form=%s&apiKey=%s&ids=%s&aggType=a&yyyy=%s&qq=%s&by=%s&retType=%s",form,apiKey,id,yyyy[i],qq[i],by,retType)
    reqstring <- req
    reqstring <- gsub(apiKey,"XXXX", reqstring)
    if(!quiet) cat("requesting: ",reqstring,"\n")

    resp <- curl_fetch_memory(req)
    if(reqs < totreqs) Sys.sleep(wait)
    if(resp$status != 200) {
      cat(" unavailable.\n")  
      return(NULL)
    }
    vsn <<- gsub(paste0(".*ownership/(.*)/",form,".*"),"\\1",resp$url)
    h <- read.csv(textConnection(rawToChar(resp$content)))
    cat("done.\n")
    h
  })

  x <- do.call(rbind, x)
  if(NROW(x) == 0) {
    x <- data.frame()
  }
  rownames(x) <- NULL
  attr(x,".vsn") <- vsn
  class(x) <- c("qk_df_beneficial","qk_df","data.frame")
  x
}
qk_insider <- function(qkid, yyyyqq, qtrs=1, form=c("insider","intent","sales"), wait=1, quiet=TRUE) {
  check_qkid(qkid)
  apiKey <- Sys.getenv("QK_API_KEY")
  if(apiKey == "") 
    stop("apiKey not found in QK_API_KEY environment variable. Set in shell or use `Sys.setenv` from R.")

  by <- "filing"
  ep <- "ownership"

  retType <- "csv"

  if(!is.numeric(qtrs)) stop("qtrs= must be integer number of qtrs to retrieve including current requested qtr")

  form <- match.arg(form)
  form <- c(insider="345",intent="144/intent",sales="144/sales")[form]
  to_from <- qtrsback(yyyyqq,qtrs)
  yyyy <- sprintf("%04d",to_from %/% 100)
  qq <- sprintf("%02d",to_from %% 100)

  id <- entity(qkid)
  ticker <- to_ticker(qkid)

  vsn <- NA
  reqs <- 1
  totreqs <- length(qq)
  x <- lapply(1:totreqs, function(i) {
    cat(sprintf("fetching %s (%s) for %s%s ...",ticker,id,yyyy[i],qq[i]))
    req <- sprintf("https://api.qkiosk.io/data/ownership?form=%s&apiKey=%s&ids=%s&aggType=a&yyyy=%s&qq=%s&by=%s&retType=%s",form,apiKey,id,yyyy[i],qq[i],by,retType)
    reqstring <- req
    reqstring <- gsub(apiKey,"XXXX", reqstring)
    if(!quiet) cat("requesting: ",reqstring,"\n")

    resp <- curl_fetch_memory(req)
    if(reqs < totreqs) Sys.sleep(wait)
    if(resp$status != 200) {
      cat(" unavailable.\n")
      return(NULL)
    }
    vsn <<- gsub(paste0(".*ownership/(.*)/",form,".*"),"\\1",resp$url)
    h <- read.csv(textConnection(rawToChar(resp$content)))
    cat("done.\n")
    h
  })

  x <- do.call(rbind, x)
  if(NROW(x) == 0) {
    x <- data.frame()
  }
  rownames(x) <- NULL
  attr(x,".vsn") <- vsn
  class(x) <- c("qk_df_insider", "qk_df","data.frame")
  x
}

qk_holders <- function(qkid, yyyyqq, qtrs=1, agg=TRUE, rollprev=TRUE, wait=1, quiet=TRUE) {
  check_qkid(qkid)

  if(to_yyyyqq(today()-qtrs(1)) == yyyyqq && isTRUE(rollprev)) {
    cat(sprintf("rolling previous filers not yet reported into current data\n"),file=stderr())
    qtrs <- 2
    wait <- 0.1
  }

  apiKey <- Sys.getenv("QK_API_KEY")
  if(apiKey == "") 
    stop("apiKey not found in QK_API_KEY environment variable. Set in shell or use `Sys.setenv` from R.")

  if(nchar(yyyyqq) != 6 && !yyyyqq %% 100 %in% 1:4)
    stop("yyyyqq must be a exactly 6 digits and last two one of 01,02,03,04 representing quarter")

  to_from <- qtrsback(yyyyqq,qtrs)
  yyyy <- sprintf("%04d",to_from %/% 100)
  qq <- sprintf("%02d",to_from %% 100)

  use_entity <- FALSE
  id <- instrument(qkid)
  if(grepl("0[40]00",cls(qkid))) {
    id <- entity(qkid)
    use_entity <- TRUE
  }

  ticker <- to_ticker(qkid)

  vsn <- NA
  reqs <- 0
  totreqs <- length(qq)
  x <- lapply(1:totreqs, function(i) {
    cat(sprintf("fetching %s (%s) for %s%s ...",ticker,id,yyyy[i],qq[i]),file=stderr())
    req <- sprintf("https://api.qkiosk.io/data/instrument?apiKey=%s&id=%s&yyyy=%s&qq=%s&entity=%s&agg=%s",apiKey,id,yyyy[i],qq[i],ifelse(use_entity,"true","false"),ifelse(agg,"true","false"))
    reqs <- reqs + 1
    rstart <- Sys.time()
    reqstring <- req
    reqstring <- gsub(apiKey,"XXXX", reqstring)
    if(!quiet) cat("requesting: ",reqstring,"\n")

    resp <- curl_fetch_memory(req)
    if(reqs < totreqs && Sys.time() - rstart < wait) Sys.sleep(wait)
    if(resp$status != 200) {
      cat("unavailable.\n")
      return(NULL)
    }
    vsn <<- gsub(".*ownership/(.*)/13f.*","\\1",resp$url)
    h <- read.csv(textConnection(rawToChar(resp$content)))
    h <- h[rev(order(h$value)),]
    cat("done.\n")
    rownames(h) <- NULL
    h
  })

  re_sort <- FALSE
  if(rollprev && qtrs == 2 && length(x) == 2) {
    currentfilers <- unique(x[[1]]$filerCik)
    x[[2]] <- x[[2]][ !x[[2]]$filerCik %in% currentfilers, ]
    re_sort <- TRUE
  }

  x <- do.call(rbind, x)
  if(NROW(x) == 0) {
    x <- data.frame()
  }
  rownames(x) <- NULL
  attr(x, ".vsn") <- vsn
  class(x) <- c("qk_df_holders","qk_df","data.frame")

  if(re_sort) x <- qk_sort(x,by='value')

  x
}

qk_institutional <- function(qkid, yyyyqq, qtrs=1, agg=TRUE, wait=0, quiet=TRUE) {
  check_qkid(qkid)
  cik <- entity(qkid)

  apiKey <- Sys.getenv("QK_API_KEY")
  if(apiKey == "") 
    stop("apiKey not found in QK_API_KEY environment variable. Set in shell or use `Sys.setenv` from R.")

  by <- "filing"
  ep <- "ownership"

  retType <- "csv"

  if(nchar(yyyyqq) != 6 && !yyyyqq %% 100 %in% 1:4)
    stop("yyyyqq must be a exactly 6 digits and last two one of 01,02,03,04 representing quarter")

  to_from <- qtrsback(yyyyqq,qtrs)
  yyyy <- sprintf("%04d",to_from %/% 100)
  qq <- sprintf("%02d",to_from %% 100)
  aggType <- ifelse(agg, "a", "n")

  vsn <- NA
  reqs <- 1
  totreqs <- length(qq)
  x <- lapply(1:totreqs, function(i) {
    cat(sprintf("fetching %s for %s%s ...",cik,yyyy[i],qq[i]))
    req <- sprintf("https://api.qkiosk.io/data/ownership?apiKey=%s&ids=%s&aggType=%s&yyyy=%s&qq=%s&by=%s&retType=%s",apiKey,cik,substr(aggType,1,1),yyyy[i],qq[i],by,retType)
    reqstring <- req
    reqstring <- gsub(apiKey,"XXXX", reqstring)
    if(!quiet) cat("requesting: ",reqstring,"\n")

    resp <- curl_fetch_memory(req)
    if(reqs < totreqs) Sys.sleep(wait)
    if(resp$status != 200) {
      cat("unavailable.\n")
      return(NULL)
    }

    vsn <<- gsub(".*ownership/(.*)/13f.*","\\1",resp$url)
    h <- read.csv(textConnection(rawToChar(resp$content)))
    cat("done.\n")
    h
  })

  x <- do.call(rbind, x)
  if(NROW(x) == 0) {
    x <- data.frame()
  }
  rownames(x) <- NULL
  attr(x,".vsn") <- vsn
  class(x) <- c("qk_df_institutional","qk_df","data.frame")
  x
}

`[.qkinstitutional` <- function(x, i, j, ...) {
  x <- x[i, j, ...]
  class(x) <- c("qk_df_institutional","qk_df","data.frame")
  x 
}

qk_sort <- function(x, ...) {
  UseMethod("qk_sort", x)
}

#   $ shrsOrPrnAmtType      : chr  "SH" "SH" "SH" "SH" ...
#   $ portWgt               : num  0.00000767 0.00000016 0 0 0.00001008 ...
#   $ otherManager          : logi  TRUE TRUE TRUE TRUE TRUE TRUE ...
#   $ QtrsHeld              : int  10 9 2 8 32 25 2 43 38 6 ...
#   $ QOQSshPrnAmt          : int  -76849 0 -8330 -15628 -122106 -133246 0 -94100 -438296 0 ...
#   $ QOQValue              : int  -824292 -2400 -457734 -73139 -2688998 -1171121 -451524 -10440737 -3024452 -1961617 ...
#   $ QOQPortWgt            : 
`qk_sort.qk_df` <- function(x, by=c('value','shrsOrPrnAmtType','portWgt','QtrsHeld', 'qtrsHeld','QOQSshPrnAmt','QOQValue','QOQPortWgt'), incr=TRUE, ...) {
  by <- match.arg(by)
  if(!isTRUE(incr))
    rev <- function(x) x
  x[rev(order(x[,by,drop=TRUE])),]
}

qk_filter <- function(x, ...) {
  UseMethod("qk_filter", x)
}

qk_filter.qk_df <- function(x, by=c('top','incr','decr','new','del'), ...) {
  if(missing(by) || by == "top")
    return(x)

  by <- match.arg(by)

  if("incr" %in% by)
    x <- x[x[,'QOQValue'] > 0 & x[,'QOQSshPrnAmt'] > 0,]
  if("decr" %in% by)
    x <- x[x[,'QOQValue'] < 0 & x[,'QOQSshPrnAmt'] < 0,]
  if("new" %in% by)
    x <- x[x[,grep("newOrDel",colnames(x))] == "NEW",]
  if("del" %in% by)
    x <- x[x[,grep("newOrDel",colnames(x))] == "DEL",]

  x
}

qk_summary <- function(x, ...) {
  UseMethod("qk_summary", x)
}
summary_cols <- function(x) {
  if(inherits(x, "qk_df_institutional")) {
    cols <- c("issuer","titleOfClass","putCall","issuerSector","issuerTicker","shrsOrPrnAmt","QOQSshPrnAmt","value","QOQValue","portWgt","QOQPortWgt","QtrsHeld","hasOtherManager")
    cnames <- c("Name","Class","putCall","Sector","Symbol","SharesOrPrincipal","QOQ (SharesOrPrincipal)","Notional $","QOQ (Notional $)","Portfolio Wgt","QOQ (Port Wgt)", "Qtrs Held", "Other Mgrs")
  } else
  if(inherits(x, "qk_df_holders")) {
    cols <- c("filerName","filerCik","issuerName","issuerTitleOfClass","putCall","shrsOrPrnAmt","QOQSshPrnAmt","value","QOQValue","portWgt","QOQPortWgt","qtrsHeld","otherManager")
    cnames <- c("Filer","Filer CIK","Name","Class","putCall","SharesOrPrincipal","QOQ (SharesOrPrincipal)","Notional $","QOQ (Notional $)","Portfolio Wgt","QOQ (Port Wgt)", "Qtrs Held", "Other Mgrs")
  } else {
    stop("unknown data class:",class(x))
  }
  x <- x[,cols]
  colnames(x) <- cnames
  x
}
qk_summary.qk_df <- function(x, n=c(10,5), ...) {
  if(length(n) == 1)
    n <- c(n,n)
  top <- head(qk_sort(x,by='value'),n[1])
  incr <- head(qk_sort(qk_filter(x,by='incr'), by='QOQValue', incr=TRUE),n[2])
  decr <- head(qk_sort(qk_filter(x,by='decr'), by='QOQValue', incr=FALSE),n[2])
  new <- head(qk_sort(qk_filter(x,by='new'), by='QOQValue', incr=TRUE),n[2])
  del <- head(qk_sort(qk_filter(x,by='del'), by='QOQValue', incr=FALSE),n[2])

  list(Top10=summary_cols(top),Top5Increased=summary_cols(incr),Top5Decreased=summary_cols(decr),Top5New=summary_cols(new),Top5Deleted=summary_cols(del))
}

print.qk_df_institutional <- function(x, ...) {
  print.qk_df(x, ..., title="QK Institutional Ownership Data Frame", showfull=TRUE, display_cols=NULL)
}
print.qk_df_insider <- function(x, ...) {
  insider_cols <- c("filedDate", "acquiredOrDisposed", "txnDate", "issuerCik","issuerName","issuerTicker","ownerCik", "ownerName", "officerTitle", "isTenPct", "form", "isTxn", "txnCodeDesc", "txnAmount", "txnPrice", "txnValue", "secTitle", "ownershipForm", "postTxnShs")
  print.qk_df(x, ..., title="QK Insider Ownership Data Frame", showfull=TRUE, display_cols=insider_cols)
}
print.qk_df_holders <- function(x, ...) {
  print.qk_df(x, ..., title="QK Holders Data Frame", showfull=TRUE, display_cols=NULL)
}
print.qk_df_beneficial <- function(x, ...) {
  print.qk_df(x, ..., title="QK Ownership Data Frame", showfull=TRUE, display_cols=NULL)
}
