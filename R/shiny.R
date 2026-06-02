#' Run CATE Shiny Application
#'
#' Launch the Shiny application for design and analysis of consistency tests
#' based on Kappa statistic with categorical responses.
#'
#' @param port The TCP port for the application. Defaults to random available port.
#' @param launch.browser Logical. Whether to launch browser automatically.
#' @param host The IPv4 address to listen on.
#' @return A Shiny application object (invisible).
#' @export
#' @examples
#' \dontrun{
#' run_cate_app()
#' }
run_cate_app <- function(port = getOption("shiny.port"),
                         launch.browser = getOption("shiny.launch.browser", interactive()),
                         host = getOption("shiny.host", "127.0.0.1")) {

  appDir <- system.file("shinyapp", package = "catekappa")

  if (appDir == "") {
    stop("Could not find Shiny app directory. Try re-installing `catekappa` package.",
         call. = FALSE)
  }

  shiny::runApp(appDir,
                port = port,
                host = host,
                launch.browser = launch.browser)
}
