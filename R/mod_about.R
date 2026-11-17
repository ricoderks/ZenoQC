#' about UI Function
#'
#' Navbar link that opens a popup with information about the app.
#'
#' @param id Internal parameter for {shiny}.
#' @noRd
mod_about_ui <- function(id) {
  ns <- NS(id)
  bslib::nav_item(
    actionLink(ns("open"), "About", icon = bsicons::bs_icon("info-circle"),
               class = "nav-link")
  )
}

#' about Server Function
#'
#' @param db_path Path of the database used by this session (shown in the popup).
#' @noRd
mod_about_server <- function(id, db_path) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$open, showModal(about_modal(db_path)))
  })
}

#' Links to the source code and the issue tracker (from DESCRIPTION)
#' @noRd
app_links <- function() {
  desc <- utils::packageDescription("zenoQC")
  first_url <- function(x, default) {
    if (is.null(x) || is.na(x)) return(default)
    trimws(strsplit(x, "[,[:space:]]+")[[1]][1])
  }
  repo <- first_url(desc$URL, "https://github.com/ricoderks/ZenoQC")
  list(repo = repo, issues = first_url(desc$BugReports, paste0(repo, "/issues")))
}

#' Content of the About popup
#'
#' @param db_path Path of the database used by this session.
#' @noRd
about_modal <- function(db_path) {
  links <- app_links()
  ext_link <- function(href, label, icon, class) {
    tags$a(href = href, target = "_blank", rel = "noopener noreferrer",
           class = paste("btn btn-sm", class), bsicons::bs_icon(icon), " ", label)
  }
  modalDialog(
    title = tags$span(
      class = "d-inline-flex align-items-center gap-2",
      tags$img(src = "www/favicon.svg", height = 28, width = 28, alt = ""),
      "About ZenoQC"
    ),
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close"),
    p("ZenoQC keeps track of the performance of the SCIEX ZenoTOF 7600+ using its",
      tags$em("Positive and Negative Quick Status Check"), "reports (.xps).",
      "All values in an uploaded report (mass accuracy, resolution, intensity,",
      "TOF calibration and detector channel alignment, with their limits and",
      "pass/fail status) are extracted and stored in a database, so the history",
      "of the instrument can be followed with interactive graphs."),
    h5("Pages", class = "mt-3"),
    tags$ul(
      tags$li(tags$strong("Overview:"), "latest positive and negative result,",
              "pass/fail history per step and the latest key values."),
      tags$li(tags$strong("Positive mode / Negative mode:"), "trends of mass accuracy,",
              "resolution, sum intensity and Zeno ratio, TOF calibration and channel",
              "alignment. Messages are shown in the hover text."),
      tags$li(tags$strong("Upload:"), "add new reports, optionally with a message",
              "(e.g. an instrument optimization performed before the check).",
              "Reports already in the database are skipped."),
      tags$li(tags$strong("History:"), "all stored reports with their values; download",
              "the original XPS file (e.g. for a service engineer), edit the message,",
              "export to CSV or delete a report.")
    ),
    h5("How to use", class = "mt-3"),
    p("Run the Quick Status Check in SCIEX OS, save the positive and/or negative",
      "report as .xps and upload the files on the", tags$strong("Upload"), "page."),
    h5("Details", class = "mt-3"),
    tags$dl(
      class = "row mb-0 small",
      tags$dt(class = "col-sm-3", "Version"),
      tags$dd(class = "col-sm-9", app_version()),
      tags$dt(class = "col-sm-3", "Database"),
      tags$dd(class = "col-sm-9", tags$code(db_path),
              tags$div(class = "text-body-secondary",
                       "SQLite; also contains the original report files."))
    ),
    div(
      class = "d-flex flex-wrap gap-2 mt-3",
      ext_link(links$repo, "Source code on GitHub", "github", "btn-outline-primary"),
      ext_link(links$issues, "Report an issue / request a feature", "bug",
               "btn-outline-secondary")
    )
  )
}
