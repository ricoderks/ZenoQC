#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = tags$span(bsicons::bs_icon("activity"), "ZenoQC"),
      window_title = "ZenoQC - ZenoTOF 7600+ Quick Check",
      theme = bslib::bs_theme(version = 5, preset = "shiny", primary = "#205D7E"),
      fillable = FALSE,
      bslib::nav_panel("Overview", icon = bsicons::bs_icon("speedometer2"),
                       mod_overview_ui("overview")),
      bslib::nav_panel("Positive mode", icon = bsicons::bs_icon("plus-circle"),
                       mod_trends_ui("pos", "Positive")),
      bslib::nav_panel("Negative mode", icon = bsicons::bs_icon("dash-circle"),
                       mod_trends_ui("neg", "Negative")),
      bslib::nav_panel("Upload", icon = bsicons::bs_icon("upload"),
                       mod_upload_ui("upload")),
      bslib::nav_panel("History", icon = bsicons::bs_icon("table"),
                       mod_history_ui("history")),
      bslib::nav_spacer(),
      bslib::nav_item(tags$span(class = "navbar-text small", "ZenoTOF 7600+"))
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "ZenoQC"
    )
  )
}
