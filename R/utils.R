#' @importFrom dplyr %>%

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


#' Import google font to raw HTML of the htmlwidget
#'
#' @param viz Visualization to which font should be added.
#' @param font Google font to import.
#'
#' @return htmlwidget with added line of font import
#' @export
import_google_font <- function(viz, opts_theme) {

  opts_fonts <- opts_theme[grepl("family", names(opts_theme))]
  opts_fonts <- Filter(Negate(is.null), opts_fonts)
  font_names <- unique(as.character(opts_fonts))

  if(length(font_names) == 0) return(viz)

  stopifnot(!is.null(viz), inherits(viz, "htmlwidget"))

  # use current id of htmlwidget if already specified
  elementId <- viz$elementId
  if(is.null(elementId)) {
    # borrow htmlwidgets unique id creator
    elementId <- sprintf(
      'htmlwidget-%s',
      htmlwidgets:::createWidgetId()
    )
    viz$elementId <- elementId
  }

  fonts_in_url <- gsub(" ", "+", font_names)

  for(font in fonts_in_url){
    viz <- htmlwidgets::prependContent(
      viz,
      htmltools::tags$link(
        href = sprintf("https://fonts.googleapis.com/css?family=%s", font),
        rel = "stylesheet"
      )
    )
  }
  viz
}

#' Add logo to reactable
#'
#' @param table Reactable object
#' @param path Path to logo png/jpg file
#' @param width Width of logo in px; default is 150
#' @param height Height of logo in px; default is NULL, resizes automatically to width.
#'
#' @return Reactable object with logo appended to htmlwidget
#' @export
add_logo_reactable <- function(table, opts_theme){

  if (!opts_theme$branding_include) return(viz)

  logo_path <- url_logo(logo = opts_theme$logo,
                        background_color = opts_theme$background_color)
  logo_width <- opts_theme$logo_width
  logo_height <- opts_theme$logo_height

  style <- 'float: right;padding-right:10px;'
  if(!is.null(logo_width)){
    style <- paste0(style, 'width:', logo_width, 'px;')
  }
  if(!is.null(logo_height)){
    style <- paste0(style, 'height:', logo_height, 'px;')
  }

  html_img <- htmltools::img(src = logo_path,
                             style = style)
  htmlwidgets::appendContent(table, html_img)
}


url_logo <- function(logo, background_color) {
  isUrl <- grepl("http", logo)
  if (isUrl) logo_url <- logo
  if (grepl("/", logo) & !isUrl) {
    logo_path <- logo
  } else {
    logo_path <- dsvizopts::local_logo_path(logo, background_color)
  }
  logo_url <- knitr::image_uri(f = logo_path)
  logo_url
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

#' Get function from string of namespace::function() to pass to do.call
getfun <- function(x) {
  if(length(grep("::", x))>0) {
    parts <- strsplit(x, "::")[[1]]
    getExportedValue(parts[1], parts[2])
  } else {
    x
  }
}

