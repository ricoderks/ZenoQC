#' upload UI Function
#'
#' Upload one or more Quick Check .xps reports and save them to the database.
#'
#' @param id Internal parameter for {shiny}.
#' @noRd
mod_upload_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_columns(
    col_widths = c(4, 8),
    bslib::card(
      bslib::card_header("Upload Quick Check reports"),
      bslib::card_body(
        p("Select one or more positive and/or negative Quick Status Check reports",
          "(.xps files exported from SCIEX OS). Reports that are already in the",
          "database are recognised and skipped."),
        fileInput(ns("files"), NULL, multiple = TRUE, accept = c(".xps"),
                  buttonLabel = "Browse...", placeholder = "No files selected"),
        actionButton(ns("save"), "Save new reports to database",
                     icon = icon("floppy-disk"), class = "btn-primary w-100")
      )
    ),
    bslib::card(
      bslib::card_header("Files"),
      DT::DTOutput(ns("preview"))
    )
  )
}

#' upload Server Function
#' @noRd
mod_upload_server <- function(id, con, db_version) {
  moduleServer(id, function(input, output, session) {
    parsed <- reactiveVal(NULL)

    observeEvent(input$files, {
      f <- input$files
      res <- withProgress(message = "Reading reports", value = 0, {
        lapply(seq_len(nrow(f)), function(k) {
          incProgress(1 / nrow(f), detail = f$name[k])
          tryCatch(
            {
              p <- parse_quickcheck(f$datapath[k], file_name = f$name[k])
              dup <- !is.na(db_find_report(con, p))
              list(file = f$name[k], parsed = p,
                   result = if (dup) "Already in database" else "New")
            },
            error = function(e) list(file = f$name[k], parsed = NULL,
                                     result = paste("Error:", conditionMessage(e)))
          )
        })
      })
      parsed(res)
    })

    output$preview <- DT::renderDT({
      res <- parsed()
      if (is.null(res)) {
        return(DT::datatable(data.frame(Message = "No files selected yet."),
                             rownames = FALSE, options = list(dom = "t")))
      }
      g <- function(x, field) if (is.null(x$parsed)) NA else x$parsed$report[[field]]
      df <- data.frame(
        File = vapply(res, `[[`, "", "file"),
        `Acquired at` = vapply(res, g, "", "acquired_at"),
        Mode = vapply(res, g, "", "polarity"),
        `Serial number` = vapply(res, g, "", "serial_number"),
        Result = vapply(res, g, "", "overall_status"),
        Database = vapply(res, `[[`, "", "result"),
        check.names = FALSE
      )
      DT::datatable(df, rownames = FALSE, selection = "none",
                    options = list(dom = "tip", pageLength = 25)) |>
        DT::formatStyle("Result", color = DT::styleEqual(c("Pass", "Fail"),
                                                         c(qc_colors$good, qc_colors$critical)),
                        fontWeight = "bold") |>
        DT::formatStyle("Database", fontWeight = DT::styleEqual("New", "bold"))
    })

    observeEvent(input$save, {
      res <- parsed()
      todo <- Filter(function(x) identical(x$result, "New"), res %||% list())
      if (!length(todo)) {
        showNotification("There are no new reports to save.", type = "warning")
        return()
      }
      out <- vapply(todo, function(x) {
        tryCatch(db_insert_report(con, x$parsed),
                 error = function(e) paste("Error:", conditionMessage(e)))
      }, character(1))
      added <- sum(out == "added")
      parsed(lapply(res, function(x) {
        if (identical(x$result, "New")) {
          r <- out[match(x$file, vapply(todo, `[[`, "", "file"))]
          x$result <- if (identical(r, "added")) "Saved" else
            if (identical(r, "duplicate")) "Already in database" else r
        }
        x
      }))
      db_version(db_version() + 1)
      showNotification(sprintf("%d report(s) saved to the database.", added),
                       type = if (added == length(todo)) "message" else "warning")
    })
  })
}
