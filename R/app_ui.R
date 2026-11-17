#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  version <- app_version()
  tagList(
    golem_add_external_resources(),
    bslib::page_navbar(
      title = tags$span(
        class = "d-inline-flex align-items-center gap-2",
        tags$img(src = "www/favicon.svg", height = 24, width = 24, alt = ""),
        "ZenoQC"
      ),
      window_title = app_title(),
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
      bslib::nav_item(
        tags$span(
          class = "navbar-text small",
          "ZenoTOF 7600+",
          tags$span(class = "badge rounded-pill text-bg-light border ms-2 fw-normal",
                    title = sprintf("zenoQC version %s", version),
                    paste0("v", version))
        )
      )
    )
  )
}

#' Version of the running app (from DESCRIPTION)
#' @noRd
app_version <- function() {
  as.character(utils::packageVersion("zenoQC"))
}

#' Browser window title, including the version
#' @noRd
app_title <- function() {
  sprintf("ZenoQC v%s - ZenoTOF 7600+ Quick Check", app_version())
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )
  tags$head(
    # browser icon: SVG for modern browsers, ICO fallback, iOS home screen
    tags$link(rel = "icon", href = "www/favicon.ico", sizes = "32x32"),
    tags$link(rel = "icon", type = "image/svg+xml", href = "www/favicon.svg"),
    tags$link(rel = "apple-touch-icon", href = "www/apple-touch-icon.png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = app_title()
    )
  )
}
