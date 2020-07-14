#' @export
showDebug <- function (hosts = "127.0.0.1") {
  hosts <- jsonlite::toJSON(hosts, auto_unbox = TRUE)
  jsCode <- paste0("$(document).ready(function(event, hosts = '", hosts, "') {
                      if (!hosts.includes(window.location.hostname)) {
                        $('<style>.shiny-output-error, .shiny-output-error:before {visibility: hidden !important;}</style>').appendTo('head');
                        $(\"[id*='debug']\").css('display', 'none');
                      }
                   })")
  tags$head(tags$script(HTML(jsCode)))
}
