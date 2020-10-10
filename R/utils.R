`%||%` <- function (x, y)
{
  if (is.empty(x))
    return(y)
  else if (is.null(x) || is.na(x))
    return(y)
  else if (class(x) == "character" && nchar(x) == 0)
    return(y)
  else x
}

is.empty <- function (x){
  !as.logical(length(x))
}



is.reactive <- function(obj){
  all(class(obj) %in% c("reactiveExpr","reactive"))
}

eval_reactives <- function(...){
  args <- list(...)
  l <- lapply(seq_along(args), function(i) {
    if (shiny::is.reactive(args[[i]]))
      return(do.call(args[[i]], list()))
    args[[i]]
  })
  if(length(l) == 1) return(l[[1]])
  names(l) <- names(args)
  l
}


loadingGif <- function(){
  "data:image/gif;base64,R0lGODlhEAALAPQAAP///wAAANra2tDQ0Orq6gYGBgAAAC4uLoKCgmBgYLq6uiIiIkpKSoqKimRkZL6+viYmJgQEBE5OTubm5tjY2PT09Dg4ONzc3PLy8ra2tqCgoMrKyu7u7gAAAAAAAAAAACH/C05FVFNDQVBFMi4wAwEAAAAh/hpDcmVhdGVkIHdpdGggYWpheGxvYWQuaW5mbwAh+QQJCwAAACwAAAAAEAALAAAFLSAgjmRpnqSgCuLKAq5AEIM4zDVw03ve27ifDgfkEYe04kDIDC5zrtYKRa2WQgAh+QQJCwAAACwAAAAAEAALAAAFJGBhGAVgnqhpHIeRvsDawqns0qeN5+y967tYLyicBYE7EYkYAgAh+QQJCwAAACwAAAAAEAALAAAFNiAgjothLOOIJAkiGgxjpGKiKMkbz7SN6zIawJcDwIK9W/HISxGBzdHTuBNOmcJVCyoUlk7CEAAh+QQJCwAAACwAAAAAEAALAAAFNSAgjqQIRRFUAo3jNGIkSdHqPI8Tz3V55zuaDacDyIQ+YrBH+hWPzJFzOQQaeavWi7oqnVIhACH5BAkLAAAALAAAAAAQAAsAAAUyICCOZGme1rJY5kRRk7hI0mJSVUXJtF3iOl7tltsBZsNfUegjAY3I5sgFY55KqdX1GgIAIfkECQsAAAAsAAAAABAACwAABTcgII5kaZ4kcV2EqLJipmnZhWGXaOOitm2aXQ4g7P2Ct2ER4AMul00kj5g0Al8tADY2y6C+4FIIACH5BAkLAAAALAAAAAAQAAsAAAUvICCOZGme5ERRk6iy7qpyHCVStA3gNa/7txxwlwv2isSacYUc+l4tADQGQ1mvpBAAIfkECQsAAAAsAAAAABAACwAABS8gII5kaZ7kRFGTqLLuqnIcJVK0DeA1r/u3HHCXC/aKxJpxhRz6Xi0ANAZDWa+kEAA7AAAAAAAAAAAA"
}





#' @export
getUrlParameters <- function(session = session) {
  parseQueryString(session$clientData$url_search)
}

file_ext <- function (x) {
  sub(".*([.*])", "\\1", basename(x))
}


from_formats_to_module <- function(formats) {
  d0 <- data.frame(download_module = c("downloadHtmlwidget", "downloadImage", "downloadTable", "downloadText"),
                   formats = c("html", "png, jpeg, svg, pdf", "csv, xlsx, json", "txt, docx, html"))
                   # stringsAsFactors = FALSE)
  l0 <- unlist(lapply(formats, function(s) {
    d0$download_module[grep(s, d0$formats)]
  }))
  t0 <- as.data.frame(table(l0), stringsAsFactors = FALSE)
  t0$l0[which.max(t0$Freq)]
}

discard_all_na_cols <- function(d){
  Filter(function(x) !all(is.na(x)), d)
}
