.QKID_UNIV <- new.env(hash=TRUE)

qk_univ <- function(univ,dt=NA,src="QK",cache=TRUE) {
  if( isTRUE(cache) && exists(univ, envir=.QKID_UNIV) ) {
    return(get(univ, envir=.QKID_UNIV))
  }
  if(src=="QK") {
    src <- "univ/QK"
    u <- .reqQKID(paste0(src,"/",univ,"/",univ),dt=dt)$qkid
  } else {
    stop("local universe files not supported yet")
  }
  if(is.null(u))
    stop("unable to retrieve universe")
  u <- qkid(u)
  assign(univ,u,envir=.QKID_UNIV)
  u
}
