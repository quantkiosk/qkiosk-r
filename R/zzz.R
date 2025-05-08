.onAttach <- function(libname, pkgname) {
  setHook(packageEvent("qkiosk", "attach"), function(...) {
    packageStartupMessage("\nWelcome to QUANTkiosk. View account details at https://quantkiosk.com/account")
    if( is.na(qk_get_apikey(req=FALSE)) )
      packageStartupMessage("\nQK API key is not set. See help(qk_set_apikey).")
  })
}

