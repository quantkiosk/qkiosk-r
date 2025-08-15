# fundamental data API access

print.qk_fn <- function(x, ...) {
  items <- unique(c(sapply(attributes(x)$qkid_item$data, names)))
  ids <- unique(names(attributes(x)$qkid_item$id))
  cat("\033[1;38;5;208mQK\033[22;39m Financials  ",length(ids)," x ",length(items),paste0("(",attributes(x)$view,")"),"\n")
  cat(.p("  \033[3mcompanies\033[0m: [", paste(head(ids),collapse=", "), ifelse(length(ids)>6,"...",""),"]\n"))
  cat(.p("  \033[3mitems\033[0m: [", paste(head(items),collapse=", "), ifelse(length(items)>6,", ...",""),"]\n"))
}

full.qk_fn <- function(x, showfull=TRUE, ...) {
  attr(x, "showfull") <- showfull
  x
}

qkiditems <- function(qkids,items,using=TRUE) {
  oqkids <- qkids
  oitems <- items
  if(length(qkids) < 1 || length(items) < 1)
    stop("requires at least one qkid and one item to be specified (e.g. qkids=qk_ticker('NVDA'), items='SALE')")
  if(!is.list(items))
    items <- list(items)
  if(length(qkids) > length(items)) 
    items <- rep(items, each=length(qkids))

  id_items <- sapply(setNames(1:length(qkids),nm=qkids$srcid), function(q) lapply(items[q], function(i) paste(entity(qkids[q]),i,sep='.')))
  data_items <- lapply(setNames(1:length(qkids), nm = sprintf("CIK%010d",as.integer(entity(qkids)))), function(q) sapply(items[[q]], function(i) {l <- list(NULL); l}))
  qkid_items <- list(id=id_items, data=data_items,qkids=oqkids,items=oitems,using=using)
  class(qkid_items) <- 'qkid_item'
  qkid_items
}
create_qkiditem <- function(fn, id, item) {
  qi <- attributes(fn)$qkid_items
  if(missing(id))
    id <- names(qi$id) 
  if(missing(item))
    item <- qi$items
  lapply(setNames(id,nm=id), function(id) {
    w_id <- which(!is.na(match(gsub("(.*?)[.].*[.](.*)", "\\1",unlist(lapply(names(qi$id), function(nm) paste(nm,qi$id[[nm]], sep = ".")))), id)))
    w_item <- which(!is.na(match(gsub("(.*?)[.].*[.](.*)", "\\2",unlist(lapply(names(qi$id), function(nm) paste(nm,qi$id[[nm]], sep = ".")))), item)))
    t(t(as.data.frame(qi$id)))[ intersect(w_id,w_item) ]
  })
}

print.qkid_item <- function(x, ...) {
  print(x$id)
}

str.qkid_item <- function(object, ...) {
  str(object$id)
}

.FNCODES <- new.env()
qk_fncodes <- function() {
  if(exists('codes',envir=.FNCODES))
    return(get('codes',envir=.FNCODES))
  fncodes <- .reqQKID("itemcodes/fn/codes")
  colnames(fncodes) <- c("stmt","code","desc")
  fncodes <- split(fncodes, fncodes$stmt)
  fncodes <- lapply(fncodes, function(.) { rownames(.) <- NULL; class(.) <- c('qk_df','data.frame'); . })
  assign('codes', fncodes, envir=.FNCODES)
  fncodes
}

qk_fn <- function(qkids,
               items,
               qkid_items=NULL,
               from=20000101,
               to=today(),
               asof=today(),
               asfiled=FALSE,
               aspit=getOption("qk.aspit",FALSE),
               ticker=TRUE,
               hide=TRUE, quiet=TRUE,
               cache=TRUE) {

  if(as.integer(from) > as.integer(to) || (as.integer(from) > 0 && as.integer(from) < 20000101))
    stop("'from' must be after 20000101 and before 'to' or a negative offset")

  view <- "asof"
  if(as.integer(asof) != as.integer(today()) || isTRUE(aspit))
    view <- "pit"
  if(isTRUE(asfiled))
    view <- "asfiled"

  if(is.null(qkid_items))
    qkid_items <- qkiditems(qkids, items, using=FALSE)

  fd <- .FD(qkid_items, view=view, ticker=ticker, hide=hide, quiet=quiet, cache=cache)
  elapsed <- attr(fd, "elapsed")
  oview <- view
  view <- attr(fd, "view")
  if(oview != view) {
    options(qk.aspit=FALSE)
    warning("Fundamental point-in-time view is not enabled for this account. Requests are converted to asof=today(). Run qk_account() for options.")
  }

  # reorder async calls to request order
  fd <- unlist(fd,recursive=FALSE)[names(unlist(qkid_items$data,recursive=FALSE))] 

  attributes(fd) <- list(elapsed=elapsed, qkid_items=qkid_items, view=view, showfull=FALSE)
  class(fd) <- c("qk_fn","list")
  return(fd)
}

.FD <- function(qkid_items, view=c('asof','asfiled','pit'), ticker=TRUE, hide=TRUE, quiet=TRUE, cache=FALSE) {
  view <- match.arg(view)
  stime <- Sys.time()

  data <- qkid_items$name

  arg_id_item <- qkid_items$id
  arg_id_item <- paste(unlist(arg_id_item), collapse=",")

  apiKey <- Sys.getenv("QK_API_KEY")
  if(apiKey == "")
    stop("apiKey not found in QK_API_KEY environment variable. Set in shell or use `Sys.setenv` from R.")

  if(isTRUE(cache)) {
    arg_id_item <- NULL
    for(qkid_item in unname(unlist(qkid_items$id))) {
      qkid_item <- strsplit(qkid_item,"\\.")[[1]]
      if( !is.null(.QKAPIDATA[["FD"]][[view]][[sprintf("CIK%010d",as.integer(qkid_item[1]))]][[qkid_item[2]]]) ) {
        data[[sprintf("CIK%010d",as.integer(qkid_item[1]))]][[qkid_item[2]]] <- .QKAPIDATA[["FD"]][[view]][[sprintf("CIK%010d",as.integer(qkid_item[1]))]][[qkid_item[2]]]
      } else {
        arg_id_item <- c(arg_id_item, paste(qkid_item, collapse="."))
      }
    }
    arg_id_item <- paste(arg_id_item, collapse=",")
  }

  if( nchar(arg_id_item) > 0) {
    req <- "https://api.qkiosk.io/data/fundamental"
    reqbody <- as.character(toJSON(list(apiKey=apiKey,id_item=arg_id_item,view=view,version=getOption("fn_version","")),auto_unbox=TRUE))
    if(!isTRUE(qkid_items$using)) {
      ids <- paste(as.character(as.integer(substring(qkid_items$qkids$qkid,1,10))),collapse=',')
      items <- paste(qkid_items$items,collapse=',')
      reqbody <- as.character(toJSON(list(apiKey=apiKey,ids=ids,items=items,view=view,version=getOption("fn_version","")),auto_unbox=TRUE))
    }
  
    reqstring <- req
    if(hide) reqstring <- gsub(apiKey,"XXXX", reqstring)
    if(!quiet) cat("requesting: ",reqstring,"\n")
  
    # using POST
    handle = new_handle()
    handle_setheaders(handle, "Content-Type"="application/json")
    handle_setheaders(handle, `Accept-Encoding` = "gzip")
    handle_setopt(handle, customrequest = "POST")
    handle_setopt(handle, postfields = reqbody)
  
    resp <- curl_fetch_memory(req, handle)
    if(resp$status_code != "200")
      stop("permission denied - verify your API key is set",call.=FALSE)
    hdrs <- parse_headers(resp$headers, multiple=TRUE)
    res <- fromJSON(rawToChar(resp$content),simplifyVector=FALSE)
    if(res$View != view) {
      ## TODO: if asof was set and full pit is unavailable to subscriber, we need to warn
    }
    view <- res$View # downgraded from pit to asof if not available
  
    if(resp$status_code != 200)
      stop(paste0("error: unable to complete request: ",paste(res$Errors,collapse=",")), call.=FALSE)
  
    fd <- list()
    success <- function(res){
      fd <<- c(fd, list(res))
    }
    failure <- function(msg){
      warning(msg)
    }
  
    lapply(unlist(res$Urls), function(req) { h <- new_handle();handle_setopt(h,connecttimeout=30);curl_fetch_multi(req, success, failure,handle=h) } )
    multi_run()
  
    status_code <- lapply(fd, function(resp) { resp$status_code })
    content_type <- lapply(fd, function(resp) { resp$type })
    headers <- lapply(fd, function(resp) { parse_headers(resp$headers, multiple=TRUE) })
    # as.POSIXct(gsub("Last-Modified: ...,.","","Last-Modified: Tue, 04 Mar 2025 01:14:10 GMT"), format="%d %b %Y %H:%M:%S", tz="GMT")
    content <- lapply(fd, function(resp) { read.csv(textConnection(rawToChar(resp$content)), colClasses=c(fq='double',fytd='double',rptq='double',rpty='double')) })
    id_item <- lapply(fd, function(resp) gsub(".*/fundamental/[a-z]+?/(.*?)/(CIK.*)[.].*[?].*", "\\2,\\1",resp$url))
  
    idItem <- res$IdItem[match(sapply(fd, function(.) .$url), unlist(res$Urls))]
    idItems_id <- gsub("^0+","",sapply(idItem, `[[`, 1))
    idItems_item <- sapply(idItem, `[[`, 2)
  
    for(i in 1:length(content)) {
      if(status_code[[i]] != 200)
        content[[i]] <- data.frame()
      .QKAPIDATA[["FD"]][[view]][[sprintf("CIK%010d",as.integer(idItems_id[i]))]][[idItems_item[i]]] <- content[[i]]
      data[[sprintf("CIK%010d",as.integer(idItems_id[i]))]][[idItems_item[i]]] <- content[[i]]
    }
  }
  etime <- Sys.time() - stime

  attr(data, "elapsed") <- etime
  attr(data, "view") <- view
  data
}

addTTM <- function(x) {
  # add FP lagged fq and fytd - e.g. fq          -> fq
  #                                    [2022,Q1] ->   [2021,Q1]
  #x <- as.data.frame(x)
  is_interval <- TRUE
  if(all(is.na(x$fytd)))
    is_interval <- FALSE
  for(q in 1:4) x$fqpy[x$fqtr==q] <- c(NA,head(x[x$fqtr==q,'fq',drop=TRUE],-1))
  for(q in 1:4) x$fytdpy[x$fqtr==q] <- c(NA,head(x[x$fqtr==q,'fytd',drop=TRUE],-1))

  x$ann <- NA
  x$ann[x$fqtr==4] <- x[x$fqtr==4,ifelse(is_interval,'fytd','fq'),drop=TRUE]
  x$lastann <- locf(x[,'ann',drop=TRUE])

  x$ttm <- ifelse(is.na(x$ann), x$lastann + x$fq - x$fqpy, x$ann)
  if(!is_interval)
    x$ttm <- rollapply(x$fq, 4, sum, fill = NA, align = "right")/4
  x <- x[,-which(colnames(x)=='lastann')]
  x
}

pitCols <- c('cyqtr','cik','item','stmt','rstmt','iq','filed','fpe','fqtr','fq','fytd','ann','ttm','fqpy','fytdpy','fqd')
## client functions for PIT
pit_asof <- pitAsOf <- function(pit, dt=today(), qtrs=1) {
  if(inherits(pit, 'qk_fn')) {
    view <- attr(pit, 'view')
    return(as.qk_df(do.call(rbind, Filter(is.data.frame,lapply(pit, function(.) pit_asof(as.qk_df(., view=view), dt=dt, qtrs=qtrs))))))
  }
  if(qtrs < 0)
    qtrs <- length(unique(to_df(pit)$fpe))
  if(inherits(dt,"Date") || inherits(dt,"POSIXt")) 
    dt <- as.integer(format(dt,"%Y%m%d"))
  pit <- pit[as.integer(pit$filed) <= as.integer(dt),] # subset to only data available on `dt`
  if(nrow(pit) == 0)
    return(NULL)
  pit <- pit[rev(!duplicated(rev(pit$fpe))),] # find dups from latest
  if(attr(pit,"view") == "pit")
    pit <- addTTM(pit)
  rownames(pit) <- NULL
  tail(pit, qtrs)
}
pit_asfiled <- pitAsFiled <- function(pit, dt=today(), qtrs=1) {
  if(inherits(pit, 'qk_fn')) {
    view <- attr(pit, 'view')
    return(as.qk_df(do.call(rbind, Filter(is.data.frame,lapply(pit, function(.) pit_asfiled(as.qk_df(., view=view), dt=dt, qtrs=qtrs))))))
  }
  if(qtrs < 0)
    qtrs <- length(unique(to_df(pit)$fpe))
  if(inherits(dt,"Date") || inherits(dt,"POSIXt")) 
    dt <- as.integer(format(dt,"%Y%m%d"))
  pit <- pit[as.integer(pit$filed) <= as.integer(dt),] # subset to only data available on `dt`
  if(nrow(pit) == 0)
    return(NULL)
  dups <- which(duplicated(pit$fpe))
  if(length(dups) > 0)
    pit <- pit[-dups,]
  if(nrow(pit) == 0)
    return(NULL)
  if(attr(pit,"view") == "pit")
    pit <- addTTM(pit)
  rownames(pit) <- NULL
  tail(pit, qtrs)
}

# INTERNAL calls
.QKAPIDATA <- new.env(hash=TRUE)

.clearQKAPIDATA <- function() {
  rm(list=ls(.QKAPIDATA), envir=.QKAPIDATA)
}
.joinfd <- function(fd, freq=c('fq','fytd','ttm')) {
  freq <- match.arg(freq)
  fdts <- NULL
  for(id in fd) {
    for(item in id) {
      fdts <- cbind(fdts,xts(item[,freq],as.Date(as.character(item$cyqtr),format="%Y%m%d")))
    }
  }
  cnames <- paste(rep(names(fd),each=length(names(fd[[1]]))), names(fd[[1]]),sep='.')
  colnames(fdts) <- cnames
  fdts
}
locf <- function(x) {
  rs <- rle(x)
  v <- rs$values
  S <- NA
  for(i in seq_along(v)) {
    if(!is.na(v[i])) {
      S <- v[i] 
    } else 
    if(is.na(v[i]) && !is.na(S)) {
      v[i] <- S
    }
  }
  rs$values <- v
  x <- inverse.rle(rs)
  x
}

#to_xts.qk_fn <- function(x, ...) xts(x[[1]][,c('fpe','filed','fq','fytd','ttm')], order.by=as.Date(as.yyyymmdd(x[[1]]$cyqtr)))
#print.qk_fn <- function(x, ...) print(x[[1]][,c('item','cyqtr','fpe','filed','fq','fytd','ttm')])
