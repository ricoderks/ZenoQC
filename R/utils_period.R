#' Period filter (presets + custom date range)
#'
#' @param ns Namespace function of the calling module.
#' @param selected Default preset.
#' @noRd
period_filter_ui <- function(ns, selected = "365") {
  bslib::layout_columns(
    col_widths = c(4, 8),
    fill = FALSE,
    shiny::selectInput(
      ns("period"), "Period",
      choices = c("Last 30 days" = "30", "Last 90 days" = "90",
                  "Last 6 months" = "182", "Last 12 months" = "365",
                  "All data" = "all", "Custom range" = "custom"),
      selected = selected
    ),
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'custom'", ns("period")),
      shiny::dateRangeInput(ns("custom_range"), "Custom range",
                            start = Sys.Date() - 365, end = Sys.Date(),
                            weekstart = 1, format = "dd-mm-yyyy")
    )
  )
}

#' Convert the period inputs into a from/to date pair
#' @noRd
period_range <- function(period, custom_range) {
  if (is.null(period) || period == "all") {
    return(list(from = NULL, to = NULL))
  }
  if (period == "custom") {
    if (is.null(custom_range) || any(is.na(custom_range))) {
      return(list(from = NULL, to = NULL))
    }
    return(list(from = as.Date(custom_range[1]), to = as.Date(custom_range[2])))
  }
  list(from = Sys.Date() - as.integer(period), to = NULL)
}
