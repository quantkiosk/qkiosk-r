qk_complete <- function(dir,subscription='raw',rollup='5m',last='5m',date=today(),fmt=c('zip','tar.gz'),gzip=TRUE,hide=TRUE,quiet=FALSE,force=FALSE) {

  dir.create(dir, showWarnings=FALSE)
  # dir should be special directory created with qk.SetFilingsDir() -> class='qkiosk_filings_dir'
  fmt <- match.arg(fmt)

  apiKey <- Sys.getenv("QK_API_KEY")
  if(apiKey == "")
    stop("apiKey not found in QK_API_KEY environment variable. Set in shell or use `Sys.setenv` from R.")

  req <- sprintf("https://api.qkiosk.io/data/filings?fmt=%s&subscription=%s&rollup=%s&date=%s&last=%s&apiKey=%s",fmt,subscription,rollup,date,last,apiKey)
  reqstring <- req
  if(hide) reqstring <- gsub(apiKey,"XXXX", reqstring)
  if(!quiet) cat("requesting: ",reqstring,"\n")

  resp <- curl_fetch_memory(req)
  res <- fromJSON(rawToChar(resp$content),simplifyVector=FALSE)
  files <- filings <- character(0)

  if(resp$status_code != 200)
    stop(resp$status_code," - ",res$Error)

  owd <- setwd(dir)
  on.exit(setwd(owd))

  ## TODO: add some sort of check maybe based on today and known rollup
  ## > seq(ISOdatetime(2024,6,28,17,9,0),ISOdatetime(2024,6,28,20,39,0),by=600)
  ## > as.POSIXlt(gsub("filings-(\\d{12})-.*$", "\\1", dir("edgar-testing/downloads/10m")),format="%Y%m%d%H%M")
  ##
  ## alternately could move logic into API endpoint where the Request sends list of rollups downloaded and returns missing ones

  dd <- file.path("downloads",rollup)
  dir.create(dd, showWarnings=FALSE, recursive=TRUE)
  urls <- unlist(res$Urls)
  filings_tofetch <- unlist(res$Filings)
  names(filings_tofetch) <- urls

  for(url in urls) {
    cat("downloading ",filings_tofetch[url], "\n")
    if(!force && file.exists(file.path(dd, basename(filings_tofetch[url]))) ) {
      message(filings_tofetch[url], "already exists. use force=TRUE to redownload") 
      next
    }
    respurl <- curl_fetch_disk(url, tempfile())
    if(respurl$status_code != "200")
      stop("permission denied - verify your API key is set", .call=FALSE)
    hdrsurl <- parse_headers(respurl$headers, multiple=TRUE)
    filename <- gsub(".*filename=(.*?)$", "\\1", grep("filename", hdrsurl[[1]], value=TRUE))
    file.copy(respurl$content, file.path(dd,filename))
    cat("copying ", filename, "\n")
    if(fmt=="zip") {
      files <- c(files, filename)
      newfilings <- unzip(file.path(dd,filename), list=TRUE)$Name
      filings <- c(filings, newfilings)
      cat("extracting",length(newfilings),"filings ...")
      unzip(file.path(dd,filename), unzip="unzip", setTimes=TRUE)
      cat("done.\n")
    }
  }
  if(gzip) {
    for(filing in filings) {
      filing <- c('-f', filing) # overwrite
      if(!quiet)
        filing <- c('-v',filing)
      system2(Sys.which('gzip'),args=filing)
    }
    filings <- paste(filings, "gz", sep=".")
  }
  cat(length(filings),"filings extracted into",dir,"\n")
  invisible(list(downloads=files,filings=filings,dir=dir))
}
