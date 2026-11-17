#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  path <- golem::get_golem_options("db_path") %||% db_path()
  con <- db_connect(path)
  session$onSessionEnded(function() DBI::dbDisconnect(con))

  # Bumped whenever reports are added or deleted, so all views refresh.
  db_version <- reactiveVal(0)

  mod_overview_server("overview", con, db_version)
  mod_trends_server("pos", con, "Positive", db_version)
  mod_trends_server("neg", con, "Negative", db_version)
  mod_upload_server("upload", con, db_version)
  mod_history_server("history", con, db_version)
  mod_about_server("about", path)
}
