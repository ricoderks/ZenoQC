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
       comment          TEXT,
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
    "CREATE TABLE IF NOT EXISTS report_files (
       report_id INTEGER PRIMARY KEY REFERENCES reports(report_id) ON DELETE CASCADE,
       file_name TEXT,
       file_md5  TEXT,
       size      INTEGER,
       content   BLOB NOT NULL
     )",
    "CREATE INDEX IF NOT EXISTS idx_sections_report ON sections(report_id)",
    "CREATE INDEX IF NOT EXISTS idx_ions_report ON ion_metrics(report_id)",
    "CREATE INDEX IF NOT EXISTS idx_align_report ON channel_alignment(report_id)"
  )
  for (s in stmts) DBI::dbExecute(con, s)

  # migrations for databases created by older versions
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(reports)")$name
  if (!"comment" %in% cols) {
    DBI::dbExecute(con, "ALTER TABLE reports ADD COLUMN comment TEXT")
  }
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
#' @param file Optional path to the original `.xps` file; when given, the
#'   file itself is stored in the database so it can be downloaded later.
#' @param comment Optional message stored with the report (e.g. "instrument
#'   optimization performed before this check").
#' @return `"added"` or `"duplicate"`.
#' @export
db_insert_report <- function(con, parsed, file = NULL, comment = NULL) {
  if (!is.na(db_find_report(con, parsed))) {
    return("duplicate")
  }
  DBI::dbWithTransaction(con, {
    rep <- parsed$report
    rep$uploaded_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    rep$comment <- clean_comment(comment)
    DBI::dbAppendTable(con, "reports", as.data.frame(rep))
    id <- DBI::dbGetQuery(con, "SELECT last_insert_rowid() AS id")$id
    if (!is.null(file)) {
      db_store_file(con, id, file, file_name = rep$file_name)
    }
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

# Blank or missing messages are stored as NULL.
clean_comment <- function(comment) {
  if (is.null(comment) || length(comment) == 0 || is.na(comment[1])) return(NA_character_)
  comment <- trimws(as.character(comment[1]))
  if (nzchar(comment)) comment else NA_character_
}

# Store (or replace) the original report file of a report.
db_store_file <- function(con, report_id, file, file_name = basename(file)) {
  content <- readBin(file, "raw", n = file.size(file))
  DBI::dbExecute(
    con,
    "INSERT OR REPLACE INTO report_files (report_id, file_name, file_md5, size, content)
     VALUES (?, ?, ?, ?, ?)",
    params = list(as.integer(report_id), file_name, unname(tools::md5sum(file)),
                  length(content), list(content))
  )
}

#' Get the original report file of a stored report
#'
#' @param con A DBI connection.
#' @param report_id Id of the report.
#' @return A list with `file_name` and `content` (raw vector), or `NULL`
#'   when the original file is not stored.
#' @export
db_get_file <- function(con, report_id) {
  res <- DBI::dbGetQuery(con, "SELECT file_name, content FROM report_files WHERE report_id = ?",
                         params = list(as.integer(report_id)))
  if (!nrow(res)) return(NULL)
  list(file_name = res$file_name[1], content = res$content[[1]])
}

#' Change the message of a stored report
#'
#' @param con A DBI connection.
#' @param report_id Id of the report.
#' @param comment New message; an empty string removes the message.
#' @return Number of updated reports.
#' @export
db_update_comment <- function(con, report_id, comment) {
  DBI::dbExecute(con, "UPDATE reports SET comment = ? WHERE report_id = ?",
                 params = list(clean_comment(comment), as.integer(report_id)))
}

#' Add original report files to reports stored without them
#'
#' Files are matched to stored reports by their MD5 checksum.
#'
#' @param dir Folder with `.xps` files.
#' @param con A DBI connection; by default the configured database is used.
#' @param recursive Also look in subfolders.
#' @return A tibble with one row per file and the result (`added`,
#'   `already stored` or `no matching report`).
#' @export
db_backfill_files <- function(dir, con = NULL, recursive = FALSE) {
  if (is.null(con)) {
    con <- db_connect()
    on.exit(DBI::dbDisconnect(con), add = TRUE)
  }
  files <- list.files(dir, pattern = "\\.xps$", full.names = TRUE,
                      recursive = recursive, ignore.case = TRUE)
  result <- vapply(files, function(f) {
    md5 <- unname(tools::md5sum(f))
    hit <- DBI::dbGetQuery(
      con,
      "SELECT r.report_id, r.file_name, f.report_id AS stored
         FROM reports r LEFT JOIN report_files f ON f.report_id = r.report_id
        WHERE r.file_md5 = ?",
      params = list(md5)
    )
    if (!nrow(hit)) return("no matching report")
    if (!is.na(hit$stored[1])) return("already stored")
    db_store_file(con, hit$report_id[1], f, file_name = hit$file_name[1])
    "added"
  }, character(1))
  tibble::tibble(file = basename(files), result = unname(result))
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
  out <- db_read(con, paste(
    "SELECT r.*, EXISTS (SELECT 1 FROM report_files f WHERE f.report_id = r.report_id) AS has_file
       FROM reports r", f$sql, "ORDER BY r.acquired_at"), f$params)
  out$has_file <- as.logical(out$has_file)
  out
}

#' @rdname db_read_data
#' @export
db_sections <- function(con, polarity = NULL, from = NULL, to = NULL) {
  f <- db_filter_sql(polarity, from, to)
  db_read(con, paste(
    "SELECT r.acquired_at, r.polarity, r.file_name, r.comment, s.*
       FROM sections s JOIN reports r ON r.report_id = s.report_id",
    f$sql, "ORDER BY r.acquired_at, s.section_order"), f$params)
}

#' @rdname db_read_data
#' @export
db_ions <- function(con, polarity = NULL, from = NULL, to = NULL) {
  f <- db_filter_sql(polarity, from, to)
  db_read(con, paste(
    "SELECT r.acquired_at, r.polarity, r.file_name, r.comment, i.*
       FROM ion_metrics i JOIN reports r ON r.report_id = i.report_id",
    f$sql, "ORDER BY r.acquired_at"), f$params)
}

#' @rdname db_read_data
#' @export
db_alignment <- function(con, polarity = NULL, from = NULL, to = NULL) {
  f <- db_filter_sql(polarity, from, to)
  db_read(con, paste(
    "SELECT r.acquired_at, r.polarity, r.file_name, r.comment, a.*
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
    tryCatch(db_insert_report(con, parse_quickcheck(f), file = f),
             error = function(e) paste("error:", conditionMessage(e)))
  }, character(1))
  tibble::tibble(file = basename(files), result = unname(result))
}
