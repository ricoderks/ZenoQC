#' history UI Function
#'
#' Table of stored reports with details, messages, download, export and delete.
#'
#' @param id Internal parameter for {shiny}.
#' @noRd
mod_history_ui <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header(
        class = "d-flex justify-content-between align-items-center flex-wrap gap-2",
        "Stored reports",
        div(
          class = "d-flex gap-2 flex-wrap",
          uiOutput(ns("download_ui"), inline = TRUE),
          actionButton(ns("edit_comment"), "Edit message", icon = icon("pen"),
                       class = "btn-outline-secondary btn-sm"),
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
      uiOutput(ns("detail_comment")),
      DT::DTOutput(ns("detail"))
    )
  )
}

#' history Server Function
#' @noRd
mod_history_server <- function(id, con, db_version) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_id <- reactiveVal(NULL)
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
        Message = truncate_text(r$comment),
        File = r$file_name,
        XPS = ifelse(r$has_file, "\u2713", ""),
        `Uploaded at` = r$uploaded_at,
        check.names = FALSE
      )
      # re-select the current report after a redraw (e.g. after editing its message)
      sel <- match(isolate(selected_id()), r$report_id)
      DT::datatable(df, rownames = FALSE, filter = "top",
                    selection = list(mode = "single",
                                     selected = if (length(sel) && !is.na(sel)) sel else NULL),
                    options = list(pageLength = 15, order = list(list(1, "desc")),
                                   columnDefs = list(list(className = "dt-center",
                                                          targets = 6)))) |>
        DT::formatStyle("Result", color = DT::styleEqual(c("Pass", "Fail"),
                                                         c(qc_colors$good, qc_colors$critical)),
                        fontWeight = "bold")
    })

    observeEvent(input$reports_rows_selected, {
      i <- input$reports_rows_selected
      selected_id(if (length(i) == 1) reports()$report_id[i] else NULL)
    }, ignoreNULL = FALSE)

    selected <- reactive({
      id <- selected_id()
      if (is.null(id)) return(NULL)
      r <- reports()
      r <- r[r$report_id == id, ]
      if (nrow(r)) r else NULL
    })

    output$download_ui <- renderUI({
      s <- selected()
      if (!is.null(s) && isTRUE(s$has_file)) {
        downloadButton(ns("download_xps"), "Download original XPS",
                       class = "btn-outline-primary btn-sm")
      } else {
        msg <- if (is.null(s)) "Select a report first" else "Original file not stored for this report"
        tags$span(
          title = msg,
          tags$button(class = "btn btn-outline-primary btn-sm", disabled = NA,
                      icon("download"), "Download original XPS")
        )
      }
    })

    output$download_xps <- downloadHandler(
      filename = function() {
        s <- selected()
        f <- if (!is.null(s)) db_get_file(con, s$report_id) else NULL
        if (is.null(f)) "report.xps" else f$file_name
      },
      content = function(file) {
        s <- selected()
        f <- if (!is.null(s)) db_get_file(con, s$report_id) else NULL
        if (is.null(f)) stop("Original file not stored for this report.")
        writeBin(f$content, file)
      },
      contentType = "application/vnd.ms-xpsdocument"
    )

    output$detail_title <- renderText({
      s <- selected()
      if (is.null(s)) "Report details (select a report above)" else
        sprintf("Report details: %s mode, %s (%s)", s$polarity,
                format(s$acquired_at, "%d %b %Y %H:%M"), s$file_name)
    })

    output$detail_comment <- renderUI({
      s <- selected()
      if (is.null(s) || is.na(s$comment)) return(NULL)
      div(class = "alert alert-secondary mx-3 mt-3 mb-0 py-2",
          bsicons::bs_icon("chat-left-text"), " ",
          tags$span(style = "white-space: pre-wrap;", s$comment))
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

    observeEvent(input$edit_comment, {
      s <- selected()
      if (is.null(s)) {
        showNotification("Select a report in the table first.", type = "warning")
        return()
      }
      showModal(modalDialog(
        title = sprintf("Message for the %s mode check of %s", tolower(s$polarity),
                        format(s$acquired_at, "%d %b %Y %H:%M")),
        textAreaInput(ns("comment_text"), NULL, value = if (is.na(s$comment)) "" else s$comment,
                      rows = 4, width = "100%",
                      placeholder = "e.g. Instrument optimization performed before this check"),
        p(class = "small text-body-secondary", "Leave empty to remove the message."),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("save_comment"), "Save", class = "btn-primary")
        )
      ))
    })

    observeEvent(input$save_comment, {
      s <- selected()
      removeModal()
      if (is.null(s)) return()
      db_update_comment(con, s$report_id, input$comment_text)
      db_version(db_version() + 1)
      showNotification("Message saved.", type = "message")
    })

    observeEvent(input$delete, {
      s <- selected()
      if (is.null(s)) {
        showNotification("Select a report in the table first.", type = "warning")
        return()
      }
      showModal(modalDialog(
        title = "Delete report?",
        p(sprintf("This removes the %s mode Quick Check of %s (%s), its stored XPS file and all its values from the database.",
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
      selected_id(NULL)
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
