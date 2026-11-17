#' trends UI Function
#'
#' Trend graphs for one polarity (Positive or Negative mode).
#'
#' @param id Internal parameter for {shiny}.
#' @param polarity `"Positive"` or `"Negative"`.
#' @noRd
mod_trends_ui <- function(id, polarity) {
  ns <- NS(id)
  plot_card <- function(title, output_id, height, info) {
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        title,
        bslib::tooltip(bsicons::bs_icon("info-circle"), info)
      ),
      plotly::plotlyOutput(ns(output_id), height = height)
    )
  }
  tagList(
    bslib::card(
      fill = FALSE,
      bslib::card_body(
        padding = c(8, 16),
        bslib::layout_columns(
          col_widths = c(8, 4),
          period_filter_ui(ns),
          uiOutput(ns("summary"))
        )
      )
    ),
    bslib::navset_card_underline(
      title = paste(polarity, "mode"),
      bslib::nav_panel(
        "Mass accuracy",
        plot_card("Mass error per calibrant ion (ppm)", "ppm", "860px",
                  "Measured mass error for each calibrant ion in the TOF MS, TOF MS/MS and TOF MS/MS Zeno on checks. The grey band is the pass range (\u00b12 ppm).")
      ),
      bslib::nav_panel(
        "Resolution",
        plot_card("Resolution per calibrant ion", "resolution", "860px",
                  "Mass resolution (FWHM) per ion. The dashed line in the same colour is the minimum resolution required for that ion.")
      ),
      bslib::nav_panel(
        "Sensitivity & Zeno",
        plot_card("Sum intensity per calibrant ion (cps, log scale)", "intensity", "860px",
                  "Summed intensity of each calibrant ion. A steady drop points to a dirty source/front end or a weaker calibrant solution. The dashed line is the minimum intensity."),
        plot_card("Zeno on / Zeno off intensity ratio", "zeno", "400px",
                  "Ratio of the summed fragment intensity with the Zeno trap on vs. off, per MS/MS ion. Tracks the Zeno trap performance over time; the dashed line marks ratio 1.")
      ),
      bslib::nav_panel(
        "Calibration",
        plot_card("TOF mass calibration", "calibration", "600px",
                  "Slope and delay of the TOF mass calibration after each check. Sudden jumps can indicate temperature or electronics changes.")
      ),
      bslib::nav_panel(
        "Channel alignment",
        plot_card("Detector channel alignment", "alignment", "600px",
                  "Slope and intercept of detector channels 1, 3 and 4 relative to the reference channel 2 (spec \u00b150).")
      )
    )
  )
}

#' trends Server Function
#'
#' @param con A DBI connection.
#' @param polarity `"Positive"` or `"Negative"`.
#' @param db_version reactiveVal that changes when the database changes.
#' @noRd
mod_trends_server <- function(id, con, polarity, db_version) {
  moduleServer(id, function(input, output, session) {
    rng <- reactive(period_range(input$period, input$custom_range))

    ions <- reactive({
      db_version()
      db_ions(con, polarity, rng()$from, rng()$to)
    })
    sections <- reactive({
      db_version()
      db_sections(con, polarity, rng()$from, rng()$to)
    })
    alignment <- reactive({
      db_version()
      db_alignment(con, polarity, rng()$from, rng()$to)
    })

    output$summary <- renderUI({
      s <- sections()
      n <- length(unique(s$report_id))
      nfail <- length(unique(s$report_id[!(s$status %in% c("Pass", "Passed"))]))
      div(
        class = "text-end pt-2",
        tags$div(class = "fs-5 fw-semibold", sprintf("%d reports", n)),
        tags$div(
          class = "text-body-secondary small",
          if (nfail > 0) {
            tagList(bsicons::bs_icon("x-octagon-fill", class = "text-danger"),
                    sprintf(" %d with a failed step", nfail))
          } else if (n > 0) {
            tagList(bsicons::bs_icon("check-circle-fill", class = "text-success"),
                    " all steps passed")
          }
        )
      )
    })

    output$ppm <- plotly::renderPlotly(
      plot_ion_metric(ions(), "error_ppm", y_title = "Mass error (ppm)", limits = "band")
    )
    output$resolution <- plotly::renderPlotly(
      plot_ion_metric(ions(), "resolution", y_title = "Resolution", limits = "min")
    )
    output$intensity <- plotly::renderPlotly(
      plot_ion_metric(ions(), "sum_intensity", y_title = "Sum intensity (cps)",
                      limits = "min", log_y = TRUE)
    )
    output$zeno <- plotly::renderPlotly(plot_zeno_ratio(ions()))
    output$calibration <- plotly::renderPlotly(plot_calibration(sections()))
    output$alignment <- plotly::renderPlotly(plot_alignment(alignment()))
  })
}
