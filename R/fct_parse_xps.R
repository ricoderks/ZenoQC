#' Extract the text tokens of an XPS document
#'
#' An XPS file is a ZIP archive with one XAML file (`*.fpage`) per page.
#' All visible text is stored in `<Glyphs UnicodeString="...">` elements.
#' Text on one line is sometimes split over several sibling `Glyphs`
#' (for example a minus sign is written as a separate glyph run), so the
#' strings of all `Glyphs` sharing the same parent element are pasted
#' together into a single token.
#'
#' @param path Path to an `.xps` file.
#' @param label File name used in error messages.
#'
#' @return Character vector with the text tokens in document order. Page
#'   boundaries are marked with the token `"#PAGEBREAK"`.
#' @export
xps_text_tokens <- function(path, label = basename(path)) {
  if (!file.exists(path)) {
    stop("File not found: ", path, call. = FALSE)
  }
  entries <- tryCatch(
    utils::unzip(path, list = TRUE)$Name,
    error = function(e) NULL,
    warning = function(w) NULL
  )
  if (is.null(entries)) {
    stop("'", label, "' is not a valid XPS file.", call. = FALSE)
  }
  pages <- grep("\\.fpage$", entries, value = TRUE)
  if (length(pages) == 0) {
    stop("'", label, "' does not contain any XPS pages.", call. = FALSE)
  }
  page_no <- as.integer(sub(".*?(\\d+)\\.fpage$", "\\1", pages))
  pages <- pages[order(page_no)]

  tmp <- tempfile("xps_")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)
  utils::unzip(path, files = pages, exdir = tmp, junkpaths = FALSE)

  unlist(lapply(pages, function(p) {
    doc <- xml2::read_xml(file.path(tmp, p))
    ns <- c(x = "http://schemas.microsoft.com/xps/2005/06")
    parents <- xml2::xml_find_all(doc, "//*[x:Glyphs]", ns = ns)
    tokens <- vapply(parents, function(node) {
      glyphs <- xml2::xml_find_all(node, "./x:Glyphs", ns = ns)
      paste(xml2::xml_attr(glyphs, "UnicodeString"), collapse = "")
    }, character(1))
    c(tokens[nzchar(tokens)], "#PAGEBREAK")
  }), use.names = FALSE)
}

# Section names that appear in a Quick Status Check report. A section header
# is always directly followed by its result ("Pass"/"Fail").
qc_sections <- c(
  "Achieve Stable Spray / Modify",
  "Channel Alignment",
  "TOF MS Mass Check",
  "TOF MS/MS Mass Check",
  "TOF MS/MS Mass Check Zeno On",
  "Save Tuning Settings"
)

# Row labels of the per-ion result tables and their short metric names.
qc_ion_rows <- c(
  "Mass (Da)" = "mass",
  "Resolution" = "resolution",
  "Sum Intensity (cps)" = "sum_intensity",
  "Error (ppm)" = "error_ppm"
)

qc_status_values <- c("Pass", "Fail", "Warning", "Failed", "Passed")

# Convert report numbers ("1e+11", "-2.00", "2.2635e+05", "-") to numeric.
qc_num <- function(x) {
  x <- trimws(gsub(",", "", x))
  x[x %in% c("", "-", "N/A", "n/a")] <- NA_character_
  suppressWarnings(as.numeric(x))
}

#' Parse a ZenoTOF 7600+ Quick Status Check report
#'
#' @param path Path to an `.xps` Quick Check report (positive or negative).
#' @param file_name Original file name to store (useful for Shiny uploads,
#'   where `path` is a temporary file). Defaults to `basename(path)`.
#'
#' @return A list with the tibbles `report` (one row), `sections`, `ions`
#'   and `alignment`.
#' @export
parse_quickcheck <- function(path, file_name = basename(path)) {
  tokens <- xps_text_tokens(path, label = file_name)
  n <- length(tokens)

  # ---- report header (repeated on every page; use the first one) ----------
  header <- qc_parse_header(tokens)
  if (is.null(header)) {
    stop("'", file_name, "' does not look like a SCIEX Instrument Tuning Report.",
         call. = FALSE)
  }
  if (!grepl("Quick Status Check", header$optimization_set, fixed = TRUE)) {
    stop("'", file_name, "' is not a Quick Status Check report (optimization set: '",
         header$optimization_set, "').", call. = FALSE)
  }
  polarity <- if (grepl("^Pos", header$optimization_set, ignore.case = TRUE)) {
    "Positive"
  } else if (grepl("^Neg", header$optimization_set, ignore.case = TRUE)) {
    "Negative"
  } else {
    stop("Could not determine the polarity of '", file_name, "'.", call. = FALSE)
  }

  # ---- walk through the tokens ---------------------------------------------
  sections <- list()
  ions <- list()
  alignment <- list()
  align_channel <- NA_integer_
  current <- NA_character_

  num_re <- "(-?[0-9.]+(?:[eE][+-]?[0-9]+)?)"
  i <- 1L
  while (i <= n) {
    tok <- tokens[i]

    if (tok %in% qc_sections && i < n && tokens[i + 1] %in% qc_status_values) {
      current <- tok
      sections[[length(sections) + 1]] <- list(
        section = tok, status = tokens[i + 1],
        slope = NA_real_, slope_prev = NA_real_,
        delay = NA_real_, delay_prev = NA_real_,
        n_spectra = NA_integer_
      )
      i <- i + 2L
      next
    }

    k <- length(sections)
    if (k > 0) {
      m <- stringr::str_match(tok, paste0("^Slope:\\s*", num_re, ",\\s*previous was\\s*", num_re))
      if (!is.na(m[1, 1])) {
        sections[[k]]$slope <- as.numeric(m[1, 2])
        sections[[k]]$slope_prev <- as.numeric(m[1, 3])
        i <- i + 1L
        next
      }
      m <- stringr::str_match(tok, paste0("^Delay:\\s*", num_re, ",\\s*previous was\\s*", num_re))
      if (!is.na(m[1, 1])) {
        sections[[k]]$delay <- as.numeric(m[1, 2])
        sections[[k]]$delay_prev <- as.numeric(m[1, 3])
        i <- i + 1L
        next
      }
      m <- stringr::str_match(tok, "^(?:Average|Sum) of\\s*(\\d+)\\s*spectra")
      if (!is.na(m[1, 1]) && is.na(sections[[k]]$n_spectra)) {
        sections[[k]]$n_spectra <- as.integer(m[1, 2])
        i <- i + 1L
        next
      }
    }

    m <- stringr::str_match(tok, "^Align to channel:\\s*(\\d+)")
    if (!is.na(m[1, 1])) {
      align_channel <- as.integer(m[1, 2])
      i <- i + 1L
      next
    }

    # Per-ion table: Item | Min | Max | Actual | Status
    if (tok == "Item" && i + 4 <= n &&
        identical(tokens[(i + 1):(i + 4)], c("Min", "Max", "Actual", "Status"))) {
      j <- i + 5L
      rows <- list()
      while (j + 4 <= n && tokens[j] %in% names(qc_ion_rows)) {
        rows[[length(rows) + 1]] <- list(
          metric = unname(qc_ion_rows[tokens[j]]),
          min = qc_num(tokens[j + 1]), max = qc_num(tokens[j + 2]),
          actual = qc_num(tokens[j + 3]), status = tokens[j + 4]
        )
        j <- j + 5L
      }
      mass_row <- Filter(function(r) r$metric == "mass", rows)
      target <- if (length(mass_row)) {
        round((mass_row[[1]]$min + mass_row[[1]]$max) / 2, 4)
      } else {
        NA_real_
      }
      for (r in rows) {
        ions[[length(ions) + 1]] <- c(list(section = current, target_mz = target), r)
      }
      i <- j
      next
    }

    # Channel alignment table: Item | Min | Max | Initial | Actual | Status
    if (tok == "Item" && i + 5 <= n &&
        identical(tokens[(i + 1):(i + 5)], c("Min", "Max", "Initial", "Actual", "Status"))) {
      j <- i + 6L
      while (j + 5 <= n && grepl("^Channel \\d+ vs\\. \\d+ ", tokens[j])) {
        cm <- stringr::str_match(tokens[j], "^Channel (\\d+) vs\\. (\\d+) (\\w+)")
        alignment[[length(alignment) + 1]] <- list(
          item = tokens[j],
          channel = as.integer(cm[1, 2]),
          reference_channel = as.integer(cm[1, 3]),
          parameter = tolower(cm[1, 4]),
          min = qc_num(tokens[j + 1]), max = qc_num(tokens[j + 2]),
          initial = qc_num(tokens[j + 3]), actual = qc_num(tokens[j + 4]),
          status = tokens[j + 5]
        )
        j <- j + 6L
      }
      i <- j
      next
    }

    i <- i + 1L
  }

  if (length(sections) == 0) {
    stop("No Quick Check results found in '", file_name, "'.", call. = FALSE)
  }

  sections_df <- dplyr::bind_rows(lapply(seq_along(sections), function(k) {
    tibble::as_tibble(c(list(section_order = k), sections[[k]]))
  }))
  ions_df <- if (length(ions)) {
    dplyr::bind_rows(lapply(ions, tibble::as_tibble))
  } else {
    tibble::tibble(section = character(), target_mz = numeric(), metric = character(),
                   min = numeric(), max = numeric(), actual = numeric(), status = character())
  }
  alignment_df <- if (length(alignment)) {
    dplyr::bind_rows(lapply(alignment, tibble::as_tibble))
  } else {
    tibble::tibble(item = character(), channel = integer(), reference_channel = integer(),
                   parameter = character(), min = numeric(), max = numeric(),
                   initial = numeric(), actual = numeric(), status = character())
  }

  overall <- if (all(sections_df$status %in% c("Pass", "Passed"))) "Pass" else "Fail"

  report <- tibble::tibble(
    acquired_at = format(header$acquired_at, "%Y-%m-%d %H:%M:%S"),
    polarity = polarity,
    optimization_set = header$optimization_set,
    instrument_name = header$instrument_name,
    instrument_model = header$instrument_model,
    serial_number = header$serial_number,
    ion_source = header$ion_source,
    overall_status = overall,
    n_pages = sum(tokens == "#PAGEBREAK"),
    align_channel = align_channel,
    file_name = file_name,
    file_md5 = unname(tools::md5sum(path))
  )

  list(report = report, sections = sections_df, ions = ions_df, alignment = alignment_df)
}

# Parse the "Instrument Tuning Report" header block.
qc_parse_header <- function(tokens) {
  txt <- paste(tokens, collapse = "|")
  sep <- "\\|?"
  val <- "([^|]*?)"
  re <- paste0(
    "Instrument Tuning Report", sep, val, sep,
    "Instrument Name:", sep, val, sep,
    "Manufacturer:", sep, val, sep,
    "Instrument Model:", sep, val, sep,
    "Target Instrument:", sep, val, sep,
    "Ion Source:", sep, val, sep,
    "Optimization Set:", sep, val, sep,
    "Serial Number:", sep, "([^|]*)"
  )
  m <- stringr::str_match(txt, re)
  if (is.na(m[1, 1])) {
    return(NULL)
  }
  m <- trimws(m[1, ])
  acquired_at <- qc_parse_datetime(m[2])
  if (is.na(acquired_at)) {
    stop("Could not read the report date '", m[2], "'.", call. = FALSE)
  }
  list(
    acquired_at = acquired_at,
    instrument_name = m[3],
    manufacturer = m[4],
    instrument_model = m[5],
    target_instrument = m[6],
    ion_source = m[7],
    optimization_set = m[8],
    serial_number = sub("www\\.SCIEX\\.com.*$", "", m[9])
  )
}

# Report dates are written as "11/17/2026 5:39 AM" (US format, instrument
# local time). They are kept as "clock time" in UTC to avoid DST shifts.
qc_parse_datetime <- function(x) {
  out <- suppressWarnings(lubridate::parse_date_time(
    x, orders = c("mdY IMp", "mdY HM", "mdY IMSp", "mdY HMS"),
    tz = "UTC", quiet = TRUE
  ))
  out
}
