#' Location of the Quick Check database
#'
#' The path is taken from the environment variable `ZENOQC_DB` or, when that
#' is not set, from `db_path` in `inst/golem-config.yml`.
#'
#' @return Normalised path to the SQLite file.
#' @export
db_path <- function() {
  path <- Sys.getenv("ZENOQC_DB", unset = "")
  if (!nzchar(path)) {
    path <- tryCatch(get_golem_config("db_path"), error = function(e) NULL)
  }
  if (is.null(path) || !nzchar(path)) {
    path <- "~/zenoqc/zenoqc.sqlite"
  }
  path.expand(path)
}

#' Open (and initialise) the Quick Check database
#'
#' @param path Path to the SQLite file. Created, including its directory,
#'   when it does not exist yet.
#'
#' @return A DBI connection. Close it with [DBI::dbDisconnect()].
#' @export
db_connect <- function(path = db_path()) {
  dir <- dirname(path)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbExecute(con, "PRAGMA busy_timeout = 5000")
  db_init(con)
  con
}

#' Create the database tables when they do not exist yet
#'
#' @param con A DBI connection.
#' @return `con`, invisibly.
#' @export
db_init <- function(con) {
  if (!identical(DBI::dbGetInfo(con)$dbname, ":memory:") &&
      nzchar(DBI::dbGetInfo(con)$dbname)) {
    DBI::dbGetQuery(con, "PRAGMA journal_mode = WAL")
  }
  stmts <- c(
    "CREATE TABLE IF NOT EXISTS reports (
       report_id        INTEGER PRIMARY KEY AUTOINCREMENT,
       acquired_at      TEXT NOT NULL,
       polarity         TEXT NOT NULL,
       optimization_set TEXT,
       instrument_name  TEXT,
       instrument_model TEXT,
       serial_number    TEXT NOT NULL,
       ion_source       TEXT,
       overall_status   TEXT,
       n_pages          INTEGER,
       align_channel    INTEGER,
       file_name        TEXT,
       file_md5         TEXT,
       uploaded_at      TEXT,
       UNIQUE (serial_number, polarity, acquired_at)
     )",
    "CREATE TABLE IF NOT EXISTS sections (
       report_id     INTEGER NOT NULL REFERENCES reports(report_id) ON DELETE CASCADE,
       section_order INTEGER,
       section       TEXT NOT NULL,
       status        TEXT,
       slope         REAL,
       slope_prev    REAL,
       delay         REAL,
       delay_prev    REAL,
       n_spectra     INTEGER
     )",
    "CREATE TABLE IF NOT EXISTS ion_metrics (
       report_id INTEGER NOT NULL REFERENCES reports(report_id) ON DELETE CASCADE,
       section   TEXT NOT NULL,
       target_mz REAL,
       metric    TEXT NOT NULL,
       min       REAL,
       max       REAL,
       actual    REAL,
       status    TEXT
     )",
    "CREATE TABLE IF NOT EXISTS channel_alignment (
       report_id         INTEGER NOT NULL REFERENCES reports(report_id) ON DELETE CASCADE,
       item              TEXT NOT NULL,
       channel           INTEGER,
       reference_channel INTEGER,
       parameter         TEXT,
       min               REAL,
       max               REAL,
       initial           REAL,
       actual            REAL,
       status            TEXT
     )",
    "CREATE INDEX IF NOT EXISTS idx_sections_report ON sections(report_id)",
    "CREATE INDEX IF NOT EXISTS idx_ions_report ON ion_metrics(report_id)",
    "CREATE INDEX IF NOT EXISTS idx_align_report ON channel_alignment(report_id)"
  )
  for (s in stmts) DBI::dbExecute(con, s)
  invisible(con)
}

#' Check whether a parsed report is already in the database
#'
#' @param con A DBI connection.
#' @param parsed Output of [parse_quickcheck()].
#' @return The `report_id` of the existing report, or `NA`.
#' @export
db_find_report <- function(con, parsed) {
  r <- parsed$report
  res <- DBI::dbGetQuery(
    con,
    "SELECT report_id FROM reports
      WHERE serial_number = ? AND polarity = ? AND acquired_at = ?",
    params = list(r$serial_number, r$polarity, r$acquired_at)
  )
  if (nrow(res)) res$report_id[1] else NA_integer_
}

#' Store a parsed Quick Check report
#'
#' A report is identified by instrument serial number, polarity and
#' acquisition time, so the same report uploaded twice (also under a
#' different file name) is stored only once.
#'
#' @param con A DBI connection.
#' @param parsed Output of [parse_quickcheck()].
#' @return `"added"` or `"duplicate"`.
#' @export
db_insert_report <- function(con, parsed) {
  if (!is.na(db_find_report(con, parsed))) {
    return("duplicate")
  }
  DBI::dbWithTransaction(con, {
    rep <- parsed$report
    rep$uploaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    DBI::dbAppendTable(con, "reports", as.data.frame(rep))
    id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    add <- function(table, df) {
      if (nrow(df)) {
        df <- as.data.frame(df)
        df$report_id <- id
        DBI::dbAppendTable(con, table, df)
      }
    }
    add("sections", parsed$sections)
    add("ion_metrics", parsed$ions)
    add("channel_alignment", parsed$alignment)
  })
  "added"
}

#' Delete a report and all its values
#'
#' @param con A DBI connection.
#' @param report_id Id(s) of the report(s) to delete.
#' @return Number of deleted reports.
#' @export
db_delete_report <- function(con, report_id) {
  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  n <- 0L
  for (id in report_id) {
    exists <- DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM reports WHERE report_id = ?",
                              params = list(as.integer(id)))$n
    if (exists > 0) {
      # the row count reported by SQLite includes the cascaded child rows
      DBI::dbExecute(con, "DELETE FROM reports WHERE report_id = ?",
                     params = list(as.integer(id)))
      n <- n + 1L
    }
  }
  n
}

# Build "WHERE" clause and parameters for polarity/date filters.
db_filter_sql <- function(polarity = NULL, from = NULL, to = NULL, alias = "r") {
  where <- character()
  params <- list()
  if (!is.null(polarity) && length(polarity)) {
    where <- c(where, sprintf("%s.polarity IN (%s)", alias,
                              paste(rep("?", length(polarity)), collapse = ", ")))
    params <- c(params, as.list(polarity))
  }
  if (!is.null(from) && !is.na(from)) {
    where <- c(where, sprintf("%s.acquired_at >= ?", alias))
    params <- c(params, format(as.Date(from), "%Y-%m-%d 00:00:00"))
  }
  if (!is.null(to) && !is.na(to)) {
    where <- c(where, sprintf("%s.acquired_at <= ?", alias))
    params <- c(params, format(as.Date(to), "%Y-%m-%d 23:59:59"))
  }
  list(
    sql = if (length(where)) paste("WHERE", paste(where, collapse = " AND ")) else "",
    params = unname(params)
  )
}

db_read <- function(con, sql, params) {
  out <- if (length(params)) {
    DBI::dbGetQuery(con, sql, params = params)
  } else {
    DBI::dbGetQuery(con, sql)
  }
  out <- tibble::as_tibble(out)
  if ("acquired_at" %in% names(out)) {
    out$acquired_at <- as.POSIXct(out$acquired_at, tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
  }
  out
}

#' Read stored Quick Check data
#'
#' @param con A DBI connection.
#' @param polarity Optional character vector (`"Positive"`, `"Negative"`).
#' @param from,to Optional dates to limit the acquisition date range.
#'
#' @return A tibble. Every table is joined with the report's acquisition
#'   time, polarity, overall status and file name.
#' @name db_read_data
#' @export
db_reports <- function(con, polarity = NULL, from = NULL, to = NULL) {
  f <- db_filter_sql(polarity, from, to)
  db_read(con, paste("SELECT r.* FROM reports r", f$sql,
                     "ORDER BY r.acquired_at"), f$params)
}

#' @rdname db_read_data
#' @export
db_sections <- function(con, polarity = NULL, from = NULL, to = NULL) {
  f <- db_filter_sql(polarity, from, to)
  db_read(con, paste(
    "SELECT r.acquired_at, r.polarity, r.file_name, s.*
       FROM sections s JOIN reports r ON r.report_id = s.report_id",
    f$sql, "ORDER BY r.acquired_at, s.section_order"), f$params)
}

#' @rdname db_read_data
#' @export
db_ions <- function(con, polarity = NULL, from = NULL, to = NULL) {
  f <- db_filter_sql(polarity, from, to)
  db_read(con, paste(
    "SELECT r.acquired_at, r.polarity, r.file_name, i.*
       FROM ion_metrics i JOIN reports r ON r.report_id = i.report_id",
    f$sql, "ORDER BY r.acquired_at"), f$params)
}

#' @rdname db_read_data
#' @export
db_alignment <- function(con, polarity = NULL, from = NULL, to = NULL) {
  f <- db_filter_sql(polarity, from, to)
  db_read(con, paste(
    "SELECT r.acquired_at, r.polarity, r.file_name, a.*
       FROM channel_alignment a JOIN reports r ON r.report_id = a.report_id",
    f$sql, "ORDER BY r.acquired_at"), f$params)
}

#' Import all Quick Check reports in a folder
#'
#' @param dir Folder with `.xps` files.
#' @param con A DBI connection; by default the configured database is used.
#' @param recursive Also look in subfolders.
#'
#' @return A tibble with one row per file and its import result
#'   (`added`, `duplicate` or `error: ...`).
#' @export
import_folder <- function(dir, con = NULL, recursive = FALSE) {
  if (is.null(con)) {
    con <- db_connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  files <- list.files(dir, pattern = "\\.xps$", full.names = TRUE,
                      recursive = recursive, ignore.case = TRUE)
  result <- vapply(files, function(f) {
    tryCatch(db_insert_report(con, parse_quickcheck(f)),
             error = function(e) paste("error:", conditionMessage(e)))
  }, character(1))
  tibble::tibble(file = basename(files), result = unname(result))
}
