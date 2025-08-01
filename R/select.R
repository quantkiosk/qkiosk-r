subset.qk_fn <- function(x, ...) {
  x1 <- subset(x[], ...)
  as.qk_df(x1, view=attr(x,"view"), showfull=attr(x,"showfull"))
}

transform.qk_fn <- function(`_data`, ...) {
  x1 <- transform(`_data`[], ...)
  as.qk_df(x1, view=attr(`_data`,"view"), showfull=attr(`_data`,"showfull"))
}

qk_select <- function(x, ...) {
  UseMethod("qk_select")
}

qk_select.qk_fn <- function(x, id_item, id, item, i, j, ...) {
## List of 5
##  $ id   :List of 1
##   ..$ CROX: chr "0001334036.SALE"
##  $ data :List of 1
##   ..$ CIK0001334036:List of 1
##   .. ..$ SALE: NULL
##  $ qkids:Class 'qkid'  hidden list of 4
##   ..$ qkid     : chr "0001334036.0000.001SDGFR8"
##   ..$ src      : chr "ticker"
##   ..$ srcid    : chr "CROX"
##   ..$ retrieved: 'yyyymmdd' int 20250618
##  $ items: chr "SALE"
##  $ using: logi FALSE

  oattr <- attributes(x)
  qi <- attr(x, "qkid_items")$id
  if(!missing(id)) {
    ## this won't work
    items <- lapply(which(sapply(x, function(.) .$cik[1]) == id),function(.) x[[.]])
  } else
  if(!missing(item)) {
    items <- lapply(which(sapply(x, function(.) .$item[1]) == item),function(.) x[[.]])
  } else
  if(missing(id_item)) {
    items <- x
  } else {
    items <- lapply(id_item, function(id_item) {
      x[[match(id_item,gsub("(.*?)[.].*[.](.*)", "\\1.\\2", unlist(lapply(names(qi), function(nm) paste(nm, qi[[nm]], sep=".")))))]]
    })
  }
  attributes(items) <- oattr
  items
}
`[.qk_fn` <- to_df.qk_fn <- function(x, id_item, id, item, i, j, ...) {
  showfull <- attr(x, 'showfull')
  view <- attr(x, 'view')
  items <- qk_select(x, id_item, id, item, i, j, ...)
  x <- do.call(rbind, items)
  if(!missing(i) &&  missing(j)) {
    x <- x[i,,drop=FALSE]
  } else if( missing(i) && !missing(j)) {
    x <- x[,j,drop=FALSE]
  } else if(!missing(i) && !missing(j)) {
    x <- x[i,j,drop=FALSE]
  }

  rownames(x) <- NULL
  
  as.qk_df(x, showfull=showfull, view=view)
}

to_ts <- function(x, ...) {
  UseMethod("to_ts")
}
as.xts.qk_ts <- function(x, ...) {
  class(x) <- c('xts','zoo')
  x
}

to_ts.qk_fn <- function(x, i='fq', dt='cyqtr', ...) {
  to_ts.qk_df(to_df(x), i=i, dt=dt)
}

to_ts.qk_df <- function(x, i='fq', dt='cyqtr', ...) {
  x <- split(x, list(x$cik, x$item))
  xx <- lapply(x, function(x) {
    xi <- x[,i]
    dts <- x[,dt,drop=TRUE]
    xts(as.matrix(xi), as.Date(yyyymmdd(dts)))
  })
  cnames <- c(sapply(strsplit(names(xx),"\\."), function(.) paste(to_ticker(qk_cik(.[1])),.[2],i,sep='.')))
  x <- do.call(cbind,xx)
  colnames(x) <- cnames
  class(x) <- c("qk_ts","xts","zoo")
  x
}
print.qk_ts <- function(x, ...) {
  df <- as.qk_df(as.data.frame(x), showfull=TRUE)
  rownames(df) <- index(as.xts(x))
  print(df)
}

to_df <- function(x, ...) {
  UseMethod("to_df")
}
to_df.qk_df <- function(x, ...) {
  x
}
to_df.data.frame <- function(x, ...) {
  class(x) <- c("qk_df",class(x))
  x
}


as.data.frame.qk_fn <- function(x, ...) {
  qk_select(x)
}

as.qk_df <- function(x, view="", showfull=FALSE, cls='qk_df') {
  class(x) <- unique(c(cls, "data.frame"))
  attr(x, "showfull") <- showfull
  attr(x, "view") <- view
  x
}

full <- function(x, showfull=TRUE, ...) {
  UseMethod("full")
}
full.qk_df <- function(x, showfull=TRUE, ...) {
  attr(x, "showfull") <- showfull
  x 
}

head.qk_df <- function(x, ...) { cls <- class(x); sf <- attr(x, "showfull"); v <- attr(x,"view"); x <- head(as.data.frame(x), ...); rownames(x) <- NULL; as.qk_df(x,view=v,showfull=sf,cls=cls) }
tail.qk_df <- function(x, ...) { cls <- class(x); sf <- attr(x, "showfull"); v <- attr(x,"view"); x <- tail(as.data.frame(x), ...); rownames(x) <- NULL; as.qk_df(x,view=v,showfull=sf,cls=cls) }
`[.qk_df` <- function(x, i, j, drop=FALSE, rerow=TRUE, ...) {
  cls <- class(x)
  x <- as.data.frame(x)
  sf <- attr(x, 'showfull')
  view <- attr(x, 'view')
  if(!missing(i) &&  missing(j)) {
    x <- x[i,,drop=drop]
  } else if( missing(i) && !missing(j)) {
    x <- x[,j,drop=drop]
  } else if(!missing(i) && !missing(j)) {
    x <- x[i,j,drop=drop]
  } else {
    sf <- attr(x, 'showfull')
  }
  if(rerow)
    rownames(x) <- NULL
  if(drop && NCOL(x)==1)
    return(x)
  as.qk_df(x, view=view,showfull=sf,cls)
}
subset.qk_df <- function(x, ...) {
  cls <- class(x)
  sf <- attributes(x)$showfull
  v <- attributes(x)$view
  x <- subset.data.frame(x, ...)
  attr(x,'showfull') <- sf
  attr(x,'view') <- v
  rownames(x) <- NULL
  class(x) <- cls
  x
}
transform.qk_df <- function(`_data`, ...) {
  cls <- class(`_data`)
  x1 <- transform.data.frame(`_data`[], ...)
  as.qk_df(x1, view=attr(`_data`,"view"), showfull=attr(`_data`,"showfull"),cls)
}

highlight <- function(x, ...) {
  UseMethod("highlight")
}
highlight.data.frame <- function(x, i, style=list(bg='yellow'), ...) {
  mc <- match.call(expand.dots=FALSE)
  if(isTRUE(is.call(mc$i))) {
    i <- with(x, eval(mc$i), ...)
  }
  attr(x,"highlight") <- structure(i, style=style)
  x
}

.qk_fn_display_cols <- 
c("cik", "acceptance_time", "stmt", "item", "filed", "fpb", "fpe", 
"fqd", "fp", "fqtr", "cyqtr", "fq", "fytd", "ttm", "ann", "rstmt"
)
.qk_fn_hidden_cols <- 
c("pik", "acceptance_utc", "unit", "form", "filename", "accn", 
"fye", "fyeq", "fpe2filed", "filed40", "ffiled", "cyear", "cqtr", 
"rptq", "rptqd", "rpty", "rptyd", "rstmt.q", "rstmt.y", "iq", 
"asof", "concept_id", "fqpy", "fytdpy")

print.qk_df <- function(x, ..., maxwidth=getOption("width"), display_cols=.qk_fn_display_cols, title="A QK Financials Data Frame", topn=getOption("qkiosk.print.topn",5), nrows=getOption("qkiosk.print.nrows",100),rerow=TRUE) {

  cat("\033[1;38;5;239m\033[3m",title,"\033[23m\033[22;39m\n")
  cat("\033[1;38;5;244m\033[3m","Data licensed by QUANTkiosk\u2122. See terms for usage restriction.","\033[23m\033[22;39m\n")

  if(rerow)
    rownames(x) <- NULL

  showfull <- attributes(x)$showfull
  highlight <- attributes(x)$highlight
  if( !all(display_cols %in% colnames(x)) )
    showfull <- TRUE

  if(nrow(x) > 0) {
    if( isTRUE(showfull) || is.null(display_cols) ) {
      cat(.style(x, highlight=highlight, topn=topn, nrows=nrows, maxwidth=maxwidth))
    } else {
      cat(.style(x[, display_cols], highlight=highlight, topn=topn, nrows=nrows, maxwidth=maxwidth))
      cat(" \033[3;38;5;244m* ",ncol(x)-length(display_cols),"hidden cols - use full() to view all",ncol(x),"columns\033[23;39m\n")
    }
  } else {
    print(data.frame())
  }
  invisible(x)
}
