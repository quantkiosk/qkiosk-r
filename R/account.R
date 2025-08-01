qk_account <- function(browser=FALSE, files=FALSE) {
  if(browser)
    return(browseURL("https://quantkiosk.com/account"))

  apiKey <- qk_get_apikey()

  nfiles <- 0L
  if(isTRUE(files))
    nfiles <- 1L

  req <- sprintf("https://api.qkiosk.io/account?apiKey=%s&nfiles=%d",apiKey,nfiles)
  resp <- curl_fetch_memory(req)
  if(resp$status_code != "200")
    stop("permission denied - verify your API key is set", call.=FALSE)

  account <- fromJSON(rawToChar(resp$content), simplifyVector=FALSE)
  class(account) <- "qkaccount"
  account
}

qk_set_apikey <- function(apiKey, verbose=TRUE) {
  Sys.setenv(QK_API_KEY=apiKey)
  if(verbose)
    message("apiKey has been set in QK_API_KEY environment variable. Set in shell configuration or place this call in your .Rprofile")
  invisible(apiKey)
}

qk_get_apikey <- function(req=TRUE) {
  apiKey <- Sys.getenv("QK_API_KEY", unset=NA)
  if(req && (is.na(apiKey) || apiKey == ""))
    stop("apiKey not found in QK_API_KEY environment variable. Set in shell or use `Sys.setenv` from R.")
  apiKey
}

print.qkaccount <- function(x, ...) {
  cat("\nQUANTkiosk Account (",x$AsOf,"):\n\n")
  cat("  Daily Quota:",x$Quota," (Hard Quota:",x$HardQuota,")\n  Daily Usage",x$Usage,"\n")
  resets <- difftime(as.POSIXct(x$AsOf,format="%A, %d-%B-%y",tz='UTC') + 86400, as.POSIXct(x$AsOf,format="%A, %d-%B-%y %H:%M:%S",tz='UTC', units='hours'))
  cat("\n  Daily quota resets in: ", sprintf("%0.2f",resets)," hours\n")
  cat("\n  Your Files (use qk_download to view or download)\n\n")
  cat("    Data:",x$DataFilesN,"\n")
  cat("    Univs:",x$UnivFilesN,"\n")
  cat("\nVisit https://quantkiosk.com/account to change your plan or explore offerings.\n\n")
}

qk_download <- function(type = c("data", "univ"), n = 10) {
  x <- qk_account(files=TRUE)

  type <- c(data="DataFiles",univ="UnivFiles")[match.arg(type)]
  choice <- menu(sapply(x[[type]], function(.) sprintf("%s\t[%s]", 
      .$File, .$LastMod))[1:n], title = sprintf("\nselect files to download (showing %d of %d)",n,x[[paste0(type,"N")]]))
  if (choice == 0) 
      return()
  url <- x[[type]][[choice]]$Url
  resp <- curl_fetch_memory(url)
  if(resp$status_code != "200")
    stop("download access not available", call.=FALSE)

  read.csv(textConnection(rawToChar(resp$content)))
}
