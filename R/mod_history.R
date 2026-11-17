#' history UI Function
#'
#' Table of stored reports with details, export and delete.
#'
#' @param id Internal parameter for {shiny}.
#' @noRd
mod_history_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center",
        "Stored reports",
        div(
          class = "d-flex gap-2",
          downloadButton(ns("export"), "Export all values (CSV)",
                         class = "btn-outline-secondary btn-sm"),
          actionButton(ns("delete"), "Delete selected report", icon = icon("trash"),
                       class = "btn-outline-danger btn-sm")
        )
      ),
      DT::DTOutput(ns("reports"))
    ),
    bslib::card(
      bslib::card_header(textOutput(ns("detail_title"), inline = TRUE)),
      DT::DTOutput(ns("detail"))
    )
  )
}

#' history Server Function
#' @noRd
mod_history_server <- function(id, con, db_version) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    reports <- reactive({
      db_version()
      r <- db_reports(con)
      r[order(r$acquired_at, decreasing = TRUE), ]
    })

    output$reports <- DT::renderDT({
      r <- reports()
      df <- data.frame(
        Id = r$report_id,
        `Acquired at` = format(r$acquired_at, "%Y-%m-%d %H:%M"),
        Mode = r$polarity,
        Result = r$overall_status,
        `Serial number` = r$serial_number,
        File = r$file_name,
        `Uploaded at` = r$uploaded_at,
        check.names = FALSE
      )
      DT::datatable(df, rownames = FALSE, selection = "single", filter = "top",
                    options = list(pageLength = 15, order = list(list(1, "desc")))) |>
        DT::formatStyle("Result", color = DT::styleEqual(c("Pass", "Fail"),
                                                         c(qc_colors$good, qc_colors$critical)),
                        fontWeight = "bold")
    })

    selected <- reactive({
      i <- input$reports_rows_selected
      if (length(i) != 1) return(NULL)
      reports()[i, ]
    })

    output$detail_title <- renderText({
      s <- selected()
      if (is.null(s)) "Report details (select a report above)" else
        sprintf("Report details: %s mode, %s (%s)", s$polarity,
                format(s$acquired_at, "%d %b %Y %H:%M"), s$file_name)
    })

    output$detail <- DT::renderDT({
      s <- selected()
      if (is.null(s)) {
        return(DT::datatable(data.frame(Message = "No report selected."), rownames = FALSE,
                             options = list(dom = "t")))
      }
      ions <- DBI::dbGetQuery(con, "SELECT * FROM ion_metrics WHERE report_id = ?",
                              params = list(s$report_id))
      w <- tidyr::pivot_wider(
        ions[, c("section", "target_mz", "metric", "actual")],
        names_from = "metric", values_from = "actual"
      )
      st <- stats::aggregate(status ~ section + target_mz, data = ions,
                             FUN = function(x) if (all(x == "Pass")) "Pass" else "Fail")
      w <- merge(w, st, by = c("section", "target_mz"), sort = FALSE)
      sec <- DBI::dbGetQuery(con, "SELECT section, section_order FROM sections WHERE report_id = ?",
                             params = list(s$report_id))
      w$ord <- sec$section_order[match(w$section, sec$section)]
      w <- w[order(w$ord, w$target_mz), ]
      col <- function(x) if (x %in% names(w)) w[[x]] else NA
      out <- data.frame(
        Step = section_label(w$section),
        `Ion (m/z)` = sprintf("%.4f", w$target_mz),
        `Measured mass` = col("mass"),
        `Error (ppm)` = col("error_ppm"),
        Resolution = col("resolution"),
        `Sum intensity (cps)` = col("sum_intensity"),
        Status = w$status,
        check.names = FALSE
      )
      DT::datatable(out, rownames = FALSE, selection = "none",
                    options = list(dom = "t", pageLength = 100, ordering = FALSE)) |>
        DT::formatStyle("Status", color = DT::styleEqual(c("Pass", "Fail"),
                                                         c(qc_colors$good, qc_colors$critical)),
                        fontWeight = "bold")
    })

    observeEvent(input$delete, {
      s <- selected()
      if (is.null(s)) {
        showNotification("Select a report in the table first.", type = "warning")
        return()
      }
      showModal(modalDialog(
        title = "Delete report?",
        p(sprintf("This removes the %s mode Quick Check of %s (%s) and all its values from the database.",
                  tolower(s$polarity), format(s$acquired_at, "%d %b %Y %H:%M"), s$file_name)),
        p("This cannot be undone. You can upload the report again later."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete"), "Delete", class = "btn-danger")
        )
      ))
    })

    observeEvent(input$confirm_delete, {
      s <- selected()
      removeModal()
      if (is.null(s)) return()
      db_delete_report(con, s$report_id)
      db_version(db_version() + 1)
      showNotification("Report deleted.", type = "message")
    })

    output$export <- downloadHandler(
      filename = function() sprintf("zenoqc_values_%s.csv", format(Sys.Date(), "%Y%m%d")),
      content = function(file) {
        ions <- db_ions(con)
        ions$acquired_at <- format(ions$acquired_at, "%Y-%m-%d %H:%M:%S")
        utils::write.csv(ions, file, row.names = FALSE)
      }
    )
  })
}
