qk_account <- function(browser=FALSE) {
  if(browser)
    return(browseURL("https://quantkiosk.com/account"))

  apiKey <- qk_get_apikey()

  req <- sprintf("https://api.qkiosk.io/account?apiKey=%s",apiKey)
  resp <- curl_fetch_memory(req)
  if(resp$status_code != "200")
    stop("permission denied - verify your API key is set", call.=FALSE)

  account <- fromJSON(rawToChar(resp$content), simplifyVector=FALSE)
  class(account) <- "qkaccount"
  account
}

qk_set_apikey <- function(apiKey) {
  Sys.setenv(QK_API_KEY=apiKey)
  message("apiKey has been set in QK_API_KEY environment variable. Set in shell configuration or place this call in your .R configuration file")
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
  cat("\nVisit https://quantkiosk.com/account to change your plan or explore offerings.\n\n")
}
