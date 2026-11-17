#' overview UI Function
#'
#' Latest status per polarity and a pass/fail timeline.
#'
#' @param id Internal parameter for {shiny}.
#' @noRd
mod_overview_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      fill = FALSE,
      uiOutput(ns("box_pos")),
      uiOutput(ns("box_neg")),
      uiOutput(ns("box_total"))
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Pass/fail history per step"),
      bslib::card_body(
        period_filter_ui(ns),
        plotly::plotlyOutput(ns("timeline"), height = "440px")
      )
    ),
    bslib::card(
      bslib::card_header("Latest key values"),
      DT::DTOutput(ns("latest"))
    )
  )
}

#' overview Server Function
#' @noRd
mod_overview_server <- function(id, con, db_version) {
  moduleServer(id, function(input, output, session) {
    reports <- reactive({
      db_version()
      db_reports(con)
    })

    status_box <- function(polarity) {
      r <- reports()
      r <- r[r$polarity == polarity, ]
      theme_none <- bslib::value_box(
        title = paste("Latest", tolower(polarity), "Quick Check"),
        value = "No reports yet",
        showcase = bsicons::bs_icon("question-circle"),
        theme = "light"
      )
      if (!nrow(r)) return(theme_none)
      last <- r[which.max(r$acquired_at), ]
      days <- as.integer(difftime(Sys.Date(), as.Date(last$acquired_at), units = "days"))
      pass <- identical(last$overall_status, "Pass")
      bslib::value_box(
        title = paste("Latest", tolower(polarity), "Quick Check"),
        value = if (pass) "Pass" else "Fail",
        showcase = bsicons::bs_icon(if (pass) "check-circle-fill" else "x-octagon-fill"),
        theme = if (pass) "success" else "danger",
        p(format(last$acquired_at, "%d %b %Y %H:%M")),
        p(if (days == 0) "today" else if (days == 1) "1 day ago" else sprintf("%d days ago", days)),
        if (!is.na(last$comment) && nzchar(last$comment)) {
          p(class = "small fst-italic mb-0", title = last$comment,
            bsicons::bs_icon("chat-left-text"), " ", truncate_text(last$comment, 80))
        }
      )
    }

    output$box_pos <- renderUI(status_box("Positive"))
    output$box_neg <- renderUI(status_box("Negative"))
    output$box_total <- renderUI({
      r <- reports()
      bslib::value_box(
        title = "Reports in database",
        value = nrow(r),
        showcase = bsicons::bs_icon("database"),
        theme = "primary",
        p(sprintf("%d positive \u00b7 %d negative", sum(r$polarity == "Positive"),
                  sum(r$polarity == "Negative"))),
        p(if (nrow(r)) sprintf("since %s", format(min(r$acquired_at), "%d %b %Y")) else "")
      )
    })

    output$timeline <- plotly::renderPlotly({
      db_version()
      rng <- period_range(input$period, input$custom_range)
      plot_status_timeline(db_sections(con, NULL, rng$from, rng$to))
    })

    output$latest <- DT::renderDT({
      r <- reports()
      if (!nrow(r)) return(DT::datatable(data.frame(Message = "No reports yet"),
                                         rownames = FALSE, options = list(dom = "t")))
      ids <- vapply(c("Positive", "Negative"), function(pol) {
        rp <- r[r$polarity == pol, ]
        if (nrow(rp)) rp$report_id[which.max(rp$acquired_at)] else NA_integer_
      }, integer(1))
      ids <- ids[!is.na(ids)]
      ions <- db_ions(con)
      ions <- ions[ions$report_id %in% ids &
                     ions$section %in% qc_mass_check_sections &
                     ions$metric %in% c("error_ppm", "resolution", "sum_intensity"), ]
      w <- tidyr::pivot_wider(
        ions[, c("polarity", "acquired_at", "section", "target_mz", "metric", "actual")],
        names_from = "metric", values_from = "actual"
      )
      st <- stats::aggregate(status ~ polarity + section + target_mz, data = ions,
                             FUN = function(x) if (all(x == "Pass")) "Pass" else "Fail")
      w <- merge(w, st, by = c("polarity", "section", "target_mz"))
      w <- w[order(w$polarity != "Positive", match(w$section, qc_mass_check_sections),
                   w$target_mz), ]
      out <- data.frame(
        Mode = w$polarity,
        Date = format(w$acquired_at, "%d-%m-%Y %H:%M"),
        Step = section_label(w$section),
        `Ion (m/z)` = sprintf("%.4f", w$target_mz),
        `Error (ppm)` = round(w$error_ppm, 2),
        Resolution = w$resolution,
        `Sum intensity (cps)` = w$sum_intensity,
        Status = w$status,
        check.names = FALSE
      )
      DT::datatable(out, rownames = FALSE,
                    options = list(dom = "t", pageLength = 50, ordering = FALSE)) |>
        DT::formatRound(c("Resolution", "Sum intensity (cps)"), digits = 0) |>
        DT::formatStyle("Status", color = DT::styleEqual(c("Pass", "Fail"),
                                                         c(qc_colors$good, qc_colors$critical)),
                        fontWeight = "bold")
    })
  })
}
