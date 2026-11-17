# Plot helpers -----------------------------------------------------------------
#
# Colour rules (from the dataviz reference palette):
# * categorical slots are used in a fixed order; every calibrant ion gets one
#   colour that it keeps in every graph (MS ions slots 1-2, MS/MS ions 3-6);
# * failed results are drawn with the "critical" status colour AND an "x"
#   symbol AND the word "Fail" in the hover text, never colour alone;
# * spec limits are neutral grey; one y-axis per panel (no dual axes).

qc_colors <- list(
  series = c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
             "#e87ba4", "#008300", "#6250d6", "#e34948"),
  good = "#0ca30c",
  critical = "#d03b3b",
  ink = "#0b0b0b",
  ink2 = "#52514e",
  muted = "#898781",
  grid = "#e1e0d9",
  axis = "#c3c2b7",
  band = "rgba(137,135,129,0.14)",
  surface = "#fcfcfb"
)

# Short labels for the report sections.
qc_section_labels <- c(
  "Achieve Stable Spray / Modify" = "Stable spray",
  "Channel Alignment" = "Channel alignment",
  "TOF MS Mass Check" = "TOF MS",
  "TOF MS/MS Mass Check" = "TOF MS/MS",
  "TOF MS/MS Mass Check Zeno On" = "TOF MS/MS Zeno on",
  "Save Tuning Settings" = "Save settings"
)

qc_mass_check_sections <- c("TOF MS Mass Check", "TOF MS/MS Mass Check",
                            "TOF MS/MS Mass Check Zeno On")

section_label <- function(x) {
  out <- unname(qc_section_labels[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

# Fixed colour per ion: MS calibrant ions first, then the MS/MS fragment ions,
# each sorted by m/z.
ion_palette <- function(ions) {
  ms <- sort(unique(round(ions$target_mz[ions$section == "TOF MS Mass Check"], 4)))
  msms <- sort(unique(round(ions$target_mz[ions$section %in%
    c("TOF MS/MS Mass Check", "TOF MS/MS Mass Check Zeno On")], 4)))
  other <- setdiff(sort(unique(round(ions$target_mz, 4))), c(ms, msms))
  mz <- unique(c(ms, msms, other))
  cols <- rep_len(qc_colors$series, length(mz))
  stats::setNames(cols, ion_label(mz))
}

ion_label <- function(mz) sprintf("m/z %.4f", mz)

fmt_num <- function(x, digits = 4) {
  ifelse(is.na(x), "-", format(signif(x, digits), big.mark = ",", scientific = FALSE,
                               trim = TRUE, drop0trailing = TRUE))
}

fmt_dt <- function(x) format(x, "%d %b %Y %H:%M")

# HTML-escape a user message and wrap it for a hover label.
escape_html <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

wrap_note <- function(x, width = 60) {
  vapply(x, function(t) {
    if (is.na(t) || !nzchar(t)) return("")
    paste(escape_html(strwrap(t, width = width)), collapse = "<br>")
  }, character(1), USE.NAMES = FALSE)
}

# "<br>Note: ..." for reports with a message, "" otherwise.
note_html <- function(comment) {
  if (is.null(comment)) return(character(0))
  w <- wrap_note(comment)
  ifelse(nzchar(w), paste0("<br><i>Note:</i> ", w), "")
}

# Placeholder shown when there is nothing to plot.
plot_empty <- function(msg = "No data yet. Upload Quick Check reports on the Upload tab.") {
  plotly::plotly_empty(type = "scatter", mode = "markers") |>
    plotly::layout(
      annotations = list(list(text = msg, showarrow = FALSE, xref = "paper", yref = "paper",
                              x = 0.5, y = 0.5, font = list(color = qc_colors$ink2, size = 14))),
      paper_bgcolor = qc_colors$surface, plot_bgcolor = qc_colors$surface
    ) |>
    plotly::config(displaylogo = FALSE)
}

qc_axis <- function(title = NULL, ...) {
  list(title = list(text = title, font = list(color = qc_colors$ink2, size = 12)),
       gridcolor = qc_colors$grid, linecolor = qc_colors$axis, zeroline = FALSE,
       tickfont = list(color = qc_colors$muted, size = 11), ...)
}

qc_layout <- function(p, legend_title = NULL) {
  p |>
    plotly::layout(
      paper_bgcolor = qc_colors$surface,
      plot_bgcolor = qc_colors$surface,
      font = list(family = "system-ui, -apple-system, 'Segoe UI', sans-serif",
                  color = qc_colors$ink),
      hovermode = "closest",
      hoverlabel = list(bgcolor = "#ffffff", bordercolor = qc_colors$axis,
                        font = list(color = qc_colors$ink)),
      legend = list(orientation = "h", x = 0, y = 1.02, yanchor = "bottom",
                    title = list(text = legend_title), font = list(size = 12)),
      margin = list(l = 70, r = 20, t = 40, b = 40)
    ) |>
    plotly::config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
                   toImageButtonOptions = list(format = "png", scale = 2))
}

# Add a band (min..max) or a stepped dashed limit line as traces, so they
# survive plotly::subplot().
add_band <- function(p, x_range, lo, hi, name = "Spec limit", showlegend = TRUE,
                     group = "spec") {
  p |>
    plotly::add_trace(
      x = c(x_range[1], x_range[2], x_range[2], x_range[1], x_range[1]),
      y = c(lo, lo, hi, hi, lo),
      type = "scatter", mode = "lines", fill = "toself",
      fillcolor = qc_colors$band, line = list(width = 0),
      hoverinfo = "skip", name = name, legendgroup = group, showlegend = showlegend,
      inherit = FALSE
    )
}

add_limit_line <- function(p, x, y, color = qc_colors$muted, name = "Spec limit",
                           showlegend = TRUE, group = "spec") {
  p |>
    plotly::add_trace(
      x = x, y = y, type = "scatter", mode = "lines",
      line = list(color = color, width = 1.5, dash = "dash", shape = "hv"),
      hoverinfo = "skip", name = name, legendgroup = group,
      showlegend = showlegend, inherit = FALSE
    )
}

add_fail_markers <- function(p, df, y = "actual", showlegend = TRUE) {
  bad <- df[!(df$status %in% c("Pass", "Passed")), , drop = FALSE]
  if (!nrow(bad)) return(p)
  p |>
    plotly::add_trace(
      data = bad, x = ~acquired_at, y = bad[[y]], text = I(bad$hover),
      type = "scatter", mode = "markers",
      marker = list(symbol = "x", size = 12, color = qc_colors$critical,
                    line = list(width = 2, color = qc_colors$critical)),
      hovertemplate = "%{text}<extra></extra>",
      name = "Fail", legendgroup = "fail", showlegend = showlegend, inherit = FALSE
    )
}

add_series <- function(p, df, y, color, name, showlegend = TRUE) {
  p |>
    plotly::add_trace(
      data = df, x = ~acquired_at, y = df[[y]], text = I(df$hover),
      type = "scatter", mode = "lines+markers",
      line = list(color = color, width = 2),
      marker = list(color = color, size = 8, line = list(color = qc_colors$surface, width = 2)),
      hovertemplate = "%{text}<extra></extra>",
      name = name, legendgroup = name, showlegend = showlegend, inherit = FALSE
    )
}

#' Trend of one per-ion metric over time
#'
#' One panel per report section, one coloured line per calibrant ion.
#'
#' @param ions Output of [db_ions()] (already filtered on polarity/date).
#' @param metric One of `"error_ppm"`, `"resolution"`, `"sum_intensity"`,
#'   `"mass"`.
#' @param sections Report sections to show (one panel each).
#' @param y_title Y-axis title.
#' @param limits `"band"` shades min..max (used for the ppm error), `"min"`
#'   draws the per-ion minimum spec as a dashed line, `"none"` draws nothing.
#' @param log_y Use a log10 y-axis.
#'
#' @return A plotly object.
#' @export
plot_ion_metric <- function(ions, metric, sections = qc_mass_check_sections,
                            y_title = metric, limits = c("none", "band", "min"),
                            log_y = FALSE) {
  limits <- match.arg(limits)
  if (is.null(ions) || !nrow(ions)) return(plot_empty())
  pal <- ion_palette(ions)
  d <- ions[ions$metric == metric & ions$section %in% sections, , drop = FALSE]
  if (!nrow(d)) return(plot_empty("No values for this metric in the selected period."))
  d$ion <- ion_label(round(d$target_mz, 4))
  d$hover <- sprintf(
    "<b>%s</b> \u00b7 %s<br>%s<br>%s: <b>%s</b><br>Spec: %s to %s<br>Status: <b>%s</b><br><span style='color:%s'>%s</span>",
    d$ion, section_label(d$section), fmt_dt(d$acquired_at), y_title,
    fmt_num(d$actual, 6), fmt_num(d$min, 6), fmt_num(d$max, 6), d$status,
    qc_colors$muted, d$file_name
  )
  d$hover <- paste0(d$hover, note_html(d$comment))
  x_range <- range(d$acquired_at)
  x_range <- x_range + c(-1, 1) * max(as.numeric(diff(x_range)) * 0.02, 43200)

  secs <- intersect(sections, unique(d$section))
  seen <- character()
  first_spec <- TRUE
  first_fail <- TRUE
  panels <- lapply(secs, function(sec) {
    ds <- d[d$section == sec, , drop = FALSE]
    p <- plotly::plot_ly()
    y_range <- NULL
    if (limits == "band") {
      lo <- stats::median(ds$min, na.rm = TRUE)
      hi <- stats::median(ds$max, na.rm = TRUE)
      if (is.finite(lo) && is.finite(hi)) {
        p <- add_band(p, x_range, lo, hi, showlegend = first_spec)
        first_spec <<- FALSE
        # leave some room around the band so it does not fill the whole panel
        yr <- range(c(lo, hi, ds$actual), na.rm = TRUE)
        y_range <- yr + c(-1, 1) * 0.2 * diff(yr)
      }
    }
    # one shared minimum for all ions -> a single neutral line
    shared_min <- limits == "min" && length(unique(stats::na.omit(ds$min))) == 1
    if (shared_min) {
      p <- add_limit_line(p, x_range, rep(stats::na.omit(ds$min)[1], 2),
                          name = "Minimum spec (dashed)", showlegend = first_spec)
      first_spec <<- FALSE
    }
    for (ion in sort(unique(ds$ion))) {
      di <- ds[ds$ion == ion, , drop = FALSE]
      if (limits == "min" && !shared_min && any(is.finite(di$min))) {
        p <- add_limit_line(p, c(di$acquired_at, x_range[2]), c(di$min, utils::tail(di$min, 1)),
                            color = pal[[ion]], name = "Minimum spec (dashed)",
                            showlegend = first_spec)
        first_spec <<- FALSE
      }
      p <- add_series(p, di, "actual", pal[[ion]], ion, showlegend = !(ion %in% seen))
      seen <<- c(seen, ion)
    }
    p <- add_fail_markers(p, ds, showlegend = first_fail)
    if (any(!(ds$status %in% c("Pass", "Passed")))) first_fail <<- FALSE
    p |> plotly::layout(
      yaxis = qc_axis(paste0(section_label(sec), "<br>", y_title),
                      type = if (log_y) "log" else "linear",
                      exponentformat = "SI", range = y_range),
      xaxis = qc_axis(NULL, range = x_range)
    )
  })

  n <- length(panels)
  p <- if (n == 1) panels[[1]] else {
    plotly::subplot(panels, nrows = n, shareX = TRUE, titleY = TRUE, margin = 0.03)
  }
  qc_layout(p)
}

#' Zeno On / Zeno Off intensity ratio per MS/MS fragment ion
#'
#' @inheritParams plot_ion_metric
#' @return A plotly object.
#' @export
plot_zeno_ratio <- function(ions) {
  if (is.null(ions) || !nrow(ions)) return(plot_empty())
  pal <- ion_palette(ions)
  si <- ions[ions$metric == "sum_intensity" &
               ions$section %in% c("TOF MS/MS Mass Check", "TOF MS/MS Mass Check Zeno On"), ]
  if (!nrow(si)) return(plot_empty("No MS/MS intensities in the selected period."))
  si$mode <- ifelse(si$section == "TOF MS/MS Mass Check Zeno On", "on", "off")
  si$target_mz <- round(si$target_mz, 4)
  w <- tidyr::pivot_wider(
    si[, intersect(c("report_id", "acquired_at", "file_name", "comment", "target_mz", "mode",
                     "actual"), names(si))],
    names_from = "mode", values_from = "actual", values_fn = mean
  )
  w <- w[is.finite(w$on) & is.finite(w$off) & w$off > 0, , drop = FALSE]
  if (!nrow(w)) return(plot_empty("No paired Zeno on/off intensities in the selected period."))
  w$ratio <- w$on / w$off
  w$ion <- ion_label(w$target_mz)
  w$status <- "Pass"
  w$hover <- sprintf(
    "<b>%s</b><br>%s<br>Zeno on / off: <b>%s</b><br>Zeno on: %s cps<br>Zeno off: %s cps<br><span style='color:%s'>%s</span>",
    w$ion, fmt_dt(w$acquired_at), fmt_num(w$ratio, 3), fmt_num(w$on, 5),
    fmt_num(w$off, 5), qc_colors$muted, w$file_name
  )
  w$hover <- paste0(w$hover, note_html(w$comment))
  w <- w[order(w$acquired_at), ]
  x_range <- range(w$acquired_at) + c(-1, 1) * 43200
  p <- plotly::plot_ly() |>
    add_limit_line(x_range, c(1, 1), color = qc_colors$muted, name = "Ratio = 1")
  for (ion in sort(unique(w$ion))) {
    p <- add_series(p, w[w$ion == ion, ], "ratio", pal[[ion]], ion)
  }
  p |>
    plotly::layout(yaxis = qc_axis("Sum intensity Zeno on / Zeno off", rangemode = "tozero"),
                   xaxis = qc_axis(NULL)) |>
    qc_layout()
}

#' TOF mass calibration (slope and delay) over time
#'
#' @param sections Output of [db_sections()].
#' @return A plotly object with a Slope panel and a Delay panel.
#' @export
plot_calibration <- function(sections) {
  if (is.null(sections) || !nrow(sections)) return(plot_empty())
  d <- sections[sections$section %in% qc_mass_check_sections & is.finite(sections$slope), ]
  if (!nrow(d)) return(plot_empty("No calibration values in the selected period."))
  d$label <- section_label(d$section)
  cols <- stats::setNames(qc_colors$series[1:3], section_label(qc_mass_check_sections))
  d$hover <- sprintf(
    "<b>%s</b><br>%s<br>Slope: <b>%.12f</b> (previous %.12f)<br>Delay: <b>%.4f</b> (previous %.4f)<br>Status: <b>%s</b><br><span style='color:%s'>%s</span>",
    d$label, fmt_dt(d$acquired_at), d$slope, d$slope_prev, d$delay, d$delay_prev,
    d$status, qc_colors$muted, d$file_name
  )
  d$hover <- paste0(d$hover, note_html(d$comment))
  mk <- function(y, title, legend) {
    p <- plotly::plot_ly()
    for (lab in intersect(names(cols), unique(d$label))) {
      p <- add_series(p, d[d$label == lab, ], y, cols[[lab]], lab, showlegend = legend)
    }
    p <- add_fail_markers(p, d, y = y, showlegend = legend)
    p |> plotly::layout(yaxis = qc_axis(title, tickformat = if (y == "slope") ".9f" else ".2f"),
                        xaxis = qc_axis(NULL))
  }
  plotly::subplot(mk("slope", "Calibration slope", TRUE), mk("delay", "Calibration delay", FALSE),
                  nrows = 2, shareX = TRUE, titleY = TRUE, margin = 0.04) |>
    qc_layout()
}

#' Detector channel alignment over time
#'
#' @param alignment Output of [db_alignment()].
#' @return A plotly object with a Slope panel and an Intercept panel.
#' @export
plot_alignment <- function(alignment) {
  if (is.null(alignment) || !nrow(alignment)) return(plot_empty())
  d <- alignment
  d$label <- sprintf("Channel %d vs %d", d$channel, d$reference_channel)
  labs <- sort(unique(d$label))
  cols <- stats::setNames(rep_len(qc_colors$series, length(labs)), labs)
  d$hover <- sprintf(
    "<b>%s %s</b><br>%s<br>Actual: <b>%s</b> (initial %s)<br>Spec: %s to %s<br>Status: <b>%s</b><br><span style='color:%s'>%s</span>",
    d$label, d$parameter, fmt_dt(d$acquired_at), fmt_num(d$actual, 6), fmt_num(d$initial, 6),
    fmt_num(d$min), fmt_num(d$max), d$status, qc_colors$muted, d$file_name
  )
  d$hover <- paste0(d$hover, note_html(d$comment))
  mk <- function(param, title, legend) {
    dp <- d[d$parameter == param, ]
    p <- plotly::plot_ly()
    for (lab in labs) {
      p <- add_series(p, dp[dp$label == lab, ], "actual", cols[[lab]], lab, showlegend = legend)
    }
    p <- add_fail_markers(p, dp, showlegend = legend)
    p |> plotly::layout(yaxis = qc_axis(title), xaxis = qc_axis(NULL))
  }
  plotly::subplot(mk("slope", "Alignment slope", TRUE), mk("intercept", "Alignment intercept", FALSE),
                  nrows = 2, shareX = TRUE, titleY = TRUE, margin = 0.04) |>
    qc_layout()
}

#' Pass/fail timeline of all report sections
#'
#' @param sections Output of [db_sections()] for one or both polarities.
#' @return A plotly object.
#' @export
plot_status_timeline <- function(sections) {
  if (is.null(sections) || !nrow(sections)) return(plot_empty())
  d <- sections[sections$section != "Save Tuning Settings", ]
  order_secs <- c("Achieve Stable Spray / Modify", "Channel Alignment", qc_mass_check_sections)
  d$row <- paste0(ifelse(d$polarity == "Positive", "Pos", "Neg"), " \u00b7 ", section_label(d$section))
  lv <- c(paste0("Neg \u00b7 ", section_label(rev(order_secs))),
          paste0("Pos \u00b7 ", section_label(rev(order_secs))))
  d$row <- factor(d$row, levels = lv)
  d$pass <- d$status %in% c("Pass", "Passed")
  d$hover <- sprintf("<b>%s</b> mode \u00b7 %s<br>%s<br>Result: <b>%s</b><br><span style='color:%s'>%s</span>",
                     d$polarity, section_label(d$section), fmt_dt(d$acquired_at), d$status,
                     qc_colors$muted, d$file_name)
  d$hover <- paste0(d$hover, note_html(d$comment))
  p <- plotly::plot_ly()
  for (ok in c(TRUE, FALSE)) {
    dd <- d[d$pass == ok, ]
    if (!nrow(dd)) next
    p <- p |> plotly::add_trace(
      data = dd, x = ~acquired_at, y = ~row, text = I(dd$hover), type = "scatter", mode = "markers",
      marker = list(symbol = if (ok) "square" else "x",
                    size = if (ok) 11 else 13,
                    color = if (ok) qc_colors$good else qc_colors$critical,
                    line = list(width = if (ok) 0 else 2, color = qc_colors$critical)),
      hovertemplate = "%{text}<extra></extra>", name = if (ok) "Pass" else "Fail",
      inherit = FALSE
    )
  }
  # one marker per report that has a message (e.g. an instrument optimization)
  if ("comment" %in% names(d)) {
    notes <- d[!is.na(d$comment) & nzchar(d$comment), , drop = FALSE]
    notes <- notes[!duplicated(notes$report_id), , drop = FALSE]
    if (nrow(notes)) {
      lv <- c(lv, "Messages")
      notes$row <- "Messages"
      notes$hover <- sprintf("<b>%s</b> mode<br>%s%s<br><span style='color:%s'>%s</span>",
                             notes$polarity, fmt_dt(notes$acquired_at), note_html(notes$comment),
                             qc_colors$muted, notes$file_name)
      p <- p |> plotly::add_trace(
        data = notes, x = ~acquired_at, y = ~row, text = I(notes$hover),
        type = "scatter", mode = "markers",
        marker = list(symbol = "diamond", size = 12, color = qc_colors$ink2,
                      line = list(width = 2, color = qc_colors$surface)),
        hovertemplate = "%{text}<extra></extra>", name = "Message", inherit = FALSE
      )
    }
  }
  p |>
    plotly::layout(
      yaxis = qc_axis(NULL, categoryorder = "array", categoryarray = lv, type = "category",
                      showgrid = FALSE),
      xaxis = qc_axis(NULL)
    ) |>
    qc_layout()
}
