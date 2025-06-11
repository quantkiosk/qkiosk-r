# new yyyymmdd class

today <- function() {
  yyyymmdd <- as.integer(format(Sys.Date(),"%Y%m%d"))
  class(yyyymmdd) <- "yyyymmdd"
  yyyymmdd
}

yyyymmdd <- function(x=Sys.time(), tz=Sys.getenv("TZ")) {
  if(is.numeric(x) && nchar(x) != 8) stop("x must be of form YYYYMMDD")
  if(missing(tz))
    tz <- Sys.getenv("TZ")
  if(tz == "")
    tz <- "UTC"
  if(is.numeric(x)) {
    yyyymmdd <- as.integer(format(as.Date(as.POSIXlt(as.character(x), format="%Y%m%d", tz=tz), tz=tz),"%Y%m%d"))
  } else {
    yyyymmdd <- as.integer(format(as.Date(as.POSIXlt(x),tz=tz),"%Y%m%d"))
  }
  class(yyyymmdd) <- "yyyymmdd"
  yyyymmdd
}

is.yyyymmdd <- function(x) inherits(x, "yyyymmdd")

as.yyyymmdd <- function(x, ...) UseMethod("as.yyyymmdd", x)
as.yyyymmdd.yyyymmdd <- function(x, ...) x
as.yyyymmdd.numeric <- function(x, ...) {
  yyyymmdd(x)
}

as.yyyymmdd.Date <- function(x, ...) { x <- as.integer(format(x, "%Y%m%d")); class(x) <- "yyyymmdd"; x }

as.Date.yyyymmdd <- function(x, ...) {
  as.Date(as.character(x), format="%Y%m%d")
}

as.POSIXct.yyyymmdd <- function(x, ...) {
  as.POSIXct(as.character(x), format="%Y%m%d")
}

as.POSIXlt.yyyymmdd <- function(x, ...) {
  as.POSIXlt(as.character(x), format="%Y%m%d")
}


Ops.yyyymmdd <- function(e1,e2) {
  e2 <- as.integer(e2)
  e1 <- get(.Generic)(as.Date(e1),e2)
  as.yyyymmdd(e1)
}

print.yyyymmdd <- function(x, ...) {
 print(unclass(x))
}

seq.yyyymmdd <- function(from, to, by, length.out = NULL, along.with = NULL, ...) {
  if(!missing(to))
    to <- as.Date(to)
  if(!is.null(along.with))
    length.out <- length(along.with)
  if(missing(by))
    by <- 'day'
  s <- seq.Date(from=as.Date(from), to, by, length.out=length.out, ...)
  as.yyyymmdd(s)
}

diff.yyyymmdd <- function(x, lag = 1, differences = 1, ...) {
    diff(as.Date(x), lag=lag, differences=differences, ...)
}

qtrsback <- function(yyyyqq, N=1) {
  if(nchar(yyyyqq)==4)
    yyyyqq <- yyyyqq * 100
  if(N==1) 
    return(yyyyqq)
  N <- N - 1L
  z <- c(4,1,2,3)[(yyyyqq %% 100 - (1:N)) %% 4 + 1]
  zz <- z
  zz[z==4] <- -1
  zz[z<4] <- 0
  zz <- cumsum(zz)
  c(yyyyqq, (yyyyqq%/%100 + zz)*100 + z)
}

qtrs <- function(n) n * 91

days <- function(n) n

to_qq <- function(yyyymmdd) {
  UseMethod("to_qq")
}
to_yyyy <- function(yyyymmdd) {
  UseMethod("to_yyyy")
}
to_yyyyqq <- function(yyyymmdd) {
  UseMethod("to_yyyyqq")
}

to_qq.yyyymmdd <- function(yyyymmdd) ((as.integer(yyyymmdd) %% 10000 %/% 100)-1) %/% 3 + 1
to_yyyy.yyyymmdd <- function(yyyymmdd) as.integer(yyyymmdd) %/% 10000
to_yyyyqq.yyyymmdd <- function(yyyymmdd) list(yyyy=to_yyyy(yyyymmdd), qq=to_qq(yyyymmdd))

from_to_yyyy_qq <- function(from,to) {
  s <- to_yyyyqq(seq.yyyymmdd(yyyymmdd(from),yyyymmdd(to)))
  yyyyqq <- s$yyyy*100 + s$qq # e.g. 202004
  u <- which(!duplicated(yyyyqq))
  list(yyyy=s$yyyy[u], qq=s$qq[u])
}
