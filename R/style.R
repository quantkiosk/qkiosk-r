.fmtcol <- function(x, big.mark=NA) { if(is.na(big.mark)) as.character(x) else format(x, big.mark=big.mark) } 
.p <- function(...) paste(..., sep="")


.theme <- list(rowid=list(fg="208"),fg=list(int="33",dbl="35",chr="235",na="248"),highlight=list(bg=228))

.pipesep <- if(l10n_info()[["UTF-8"]]) { "\u2502" } else { "|" }
.style <- function(x, colsep=.pipesep, highlight, theme=.theme, maxwidth=getOption("width"), topn=getOption("qkiosk.df.topn",5), nrows=getOption("qkiosk.df.nrows",100)) {
  hl <- rep(FALSE, len=nrow(x))
  if( !missing(highlight) ) {
    if(is.numeric(highlight) || is.logical(highlight)) {
      hl[highlight] <- TRUE
    }
  }

  middle <- NA
  prows <- rows <- 1:nrow(x)
  if(nrows < nrow(x)) {
    rows <- c(1:topn,(nrow(x)-topn):nrow(x))
    prows <- c(1:topn,(nrow(x)-topn+1):nrow(x))
    middle <- topn+1
  }

  padding <- nchar(colsep)
  ncharna <- function(ch) ifelse(is.na(ch), 2, nchar(ch))
  max_w <- header <- styled <- NULL
  cnames <- sprintf("%-5s",colnames(x))
  rowchars <- max(3, nchar(rownames(x)))
  maxwidth <- maxwidth - 20
  header[[1]] <- paste0(rep(" ", rowchars), collapse="") # TODO: make QK
  for(j in 1:ncol(x)) {
     max_w[j] <- max(ncharna(x[[j]][rows]), nchar(.fmtcol(cnames[j]))) + padding
     hdr_style <- sprintf("%%-%ds", max(3,ncharna(.fmtcol(x[[j]][rows])), nchar(cnames[j])))
     header[[j+1]] <- sprintf(hdr_style, cnames[j])
  }

  cbreaks <- cumsum(max_w) %/% as.integer(maxwidth)
  w_breaks <-split(1:(ncol(x)+1), c(0,cbreaks))
  w_breaks[[1]] <- w_breaks[[1]][-1]
  row_style <- sprintf("\033[38;5;%sm%% %ds\033[39m", theme$rowid$fg, max(3,nchar(rownames(x))))
  styled[[1]] <- sprintf(row_style, rownames(x)[rows])
  for(j in 1:ncol(x)) {
     fmt <- ifelse(is.character(x[[j]][rows]), .p("\033[3;38;5;",theme$fg$chr,"m%%-%ds\033[23;39m"), ifelse(is.integer(x[[j]][rows]), .p("\033[38;5;",theme$fg$int,"m%% %ds\033[39m"), .p("\033[38;5;",theme$fg$dbl,"m%% %ds\033[39m")))
     fmt <- ifelse(is.na(x[[j]][rows]), ifelse(is.character(x[[j]][rows]),"\033[3;38;2;248m%%-%ds\033[22;39m","\033[38;5;248m%% %ds\033[39m"), fmt)
     col_style <- sprintf(fmt, max(ncharna(x[[j]][rows]), nchar(cnames[j])))
     styled[[j+1]] <- sprintf(col_style, .fmtcol(x[[j]][rows]))
  }
  styled_rows <- rep(list(NULL), length(w_breaks))

  for(i in 1:length(w_breaks)) {
    styled_rows[[i]][1] <- paste0('\033[48;5;017m\033[38;5;231m',paste(sapply(header[c(1,w_breaks[[i]])], `[`, 1), collapse=colsep), '\033[39;0m\033[49;0m\n')
    ii <- 1
    for(row in 1:length(rows)) {

      if(row %% 2 == 0) {
        styled_rows[[i]][ii+1] <- paste0("\033[48;5;255m",paste(sapply(styled[c(1,w_breaks[[i]])], `[`, row), collapse=colsep), '\033[49;0m\n')
      } else {
        # terminals may view control characters as taking up space - pad exactly
        styled_rows[[i]][ii+1] <- paste0("\033[48;5;231m",paste(sapply(styled[c(1,w_breaks[[i]])], `[`, row), collapse=colsep), '\033[00;0m\n')
      }
      if(isTRUE(hl[row]))
        styled_rows[[i]][ii+1] <- paste0("\033[48;5;",theme$highlight$bg,"m",paste(sapply(styled[c(1,w_breaks[[i]])], `[`, row), collapse=colsep), '\033[49;0m\n')

      if(!is.na(middle) && row==middle)
        styled_rows[[i]][ii+1] <- "  ----\n"

      ii <- ii + 1
    }
    # only show colnames at bottom if displaying non-truncated table that does not wrap
    #if(is.na(middle) && nrows < nrow(x) && length(w_breaks)==1 && getOption("qkiosk.df.colnames.after",TRUE))
    #  styled_rows[[i]][ii+1] <- paste0('\033[48;5;017m\033[38;1;097m',paste(sapply(header[c(1,w_breaks[[i]])], `[`, 1), collapse=colsep), '\033[39;0m\033[49;0m\n')
  }
  x <- paste0(unlist(styled_rows,recursive=FALSE), collapse='')
  attr(x,'styled') <- styled
  class(x) <- '.qkstyled'
  x
}
