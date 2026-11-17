fixture <- function(x) testthat::test_path("fixtures", x)

test_that("reports are stored once and deleted with all values", {
  path <- tempfile(fileext = ".sqlite")
  con <- db_connect(path)
  on.exit({ DBI::dbDisconnect(con); unlink(path) })

  pos <- parse_quickcheck(fixture("pos_quickcheck.xps"))
  neg <- parse_quickcheck(fixture("neg_quickcheck.xps"))
  expect_equal(db_insert_report(con, pos), "added")
  expect_equal(db_insert_report(con, neg), "added")
  expect_equal(db_insert_report(con, pos), "duplicate")

  # renamed copy of the same report is still a duplicate
  pos2 <- pos
  pos2$report$file_name <- "renamed.xps"
  expect_equal(db_insert_report(con, pos2), "duplicate")

  r <- db_reports(con)
  expect_equal(nrow(r), 2)
  expect_s3_class(r$acquired_at, "POSIXct")
  expect_equal(nrow(db_ions(con, "Positive")), 50)
  expect_equal(nrow(db_sections(con, "Negative")), 5)
  expect_equal(nrow(db_alignment(con)), 12)
  expect_equal(nrow(db_ions(con, from = as.Date("2026-11-10"))), 50)
  expect_equal(nrow(db_ions(con, to = as.Date("2026-11-10"))), 50)

  id <- r$report_id[r$polarity == "Positive"]
  expect_equal(db_delete_report(con, id), 1)
  expect_equal(nrow(db_reports(con)), 1)
  for (tb in c("sections", "ion_metrics", "channel_alignment")) {
    n <- DBI::dbGetQuery(con, sprintf("SELECT COUNT(*) AS n FROM %s WHERE report_id = ?", tb),
                         params = list(id))$n
    expect_equal(n, 0)
  }
})

test_that("import_folder imports and skips duplicates", {
  con <- db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  res <- import_folder(testthat::test_path("fixtures"), con = con)
  expect_equal(sort(res$result), c("added", "added"))
  res <- import_folder(testthat::test_path("fixtures"), con = con)
  expect_equal(res$result, c("duplicate", "duplicate"))
})

test_that("original files and messages are stored", {
  con <- db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  f <- fixture("pos_quickcheck.xps")
  expect_equal(db_insert_report(con, parse_quickcheck(f), file = f,
                                comment = "  Optimization done  "), "added")
  expect_equal(db_insert_report(con, parse_quickcheck(fixture("neg_quickcheck.xps")),
                                comment = "   "), "added")

  r <- db_reports(con)
  pos <- r[r$polarity == "Positive", ]
  neg <- r[r$polarity == "Negative", ]
  expect_equal(pos$comment, "Optimization done")
  expect_true(is.na(neg$comment))
  expect_true(pos$has_file)
  expect_false(neg$has_file)
  expect_equal(unique(db_ions(con, "Positive")$comment), "Optimization done")

  stored <- db_get_file(con, pos$report_id)
  expect_equal(stored$file_name, "pos_quickcheck.xps")
  expect_identical(stored$content, readBin(f, "raw", file.size(f)))
  expect_null(db_get_file(con, neg$report_id))

  expect_equal(db_update_comment(con, neg$report_id, "Source cleaned"), 1)
  expect_equal(db_reports(con, "Negative")$comment, "Source cleaned")
  db_update_comment(con, neg$report_id, "")
  expect_true(is.na(db_reports(con, "Negative")$comment))

  db_delete_report(con, pos$report_id)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) AS n FROM report_files")$n, 0)
})

test_that("db_backfill_files adds files to reports stored without them", {
  con <- db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  db_insert_report(con, parse_quickcheck(fixture("pos_quickcheck.xps")))
  db_insert_report(con, parse_quickcheck(fixture("neg_quickcheck.xps")))
  expect_false(any(db_reports(con)$has_file))

  res <- db_backfill_files(testthat::test_path("fixtures"), con = con)
  expect_equal(res$result, c("added", "added"))
  expect_true(all(db_reports(con)$has_file))
  res <- db_backfill_files(testthat::test_path("fixtures"), con = con)
  expect_equal(res$result, c("already stored", "already stored"))

  other <- tempfile(fileext = ".xps")
  writeLines("something else", other)
  res <- db_backfill_files(dirname(other), con = con)
  expect_equal(res$result[res$file == basename(other)], "no matching report")
})

test_that("databases from the first version are upgraded", {
  path <- tempfile(fileext = ".sqlite")
  old <- DBI::dbConnect(RSQLite::SQLite(), path)
  DBI::dbExecute(old, "CREATE TABLE reports (
    report_id INTEGER PRIMARY KEY AUTOINCREMENT, acquired_at TEXT NOT NULL,
    polarity TEXT NOT NULL, optimization_set TEXT, instrument_name TEXT,
    instrument_model TEXT, serial_number TEXT NOT NULL, ion_source TEXT,
    overall_status TEXT, n_pages INTEGER, align_channel INTEGER, file_name TEXT,
    file_md5 TEXT, uploaded_at TEXT, UNIQUE (serial_number, polarity, acquired_at))")
  DBI::dbExecute(old, "INSERT INTO reports (acquired_at, polarity, serial_number)
                       VALUES ('2026-01-01 08:00:00', 'Positive', 'X1')")
  DBI::dbDisconnect(old)

  con <- db_connect(path)
  on.exit(DBI::dbDisconnect(con))
  expect_true("comment" %in% DBI::dbGetQuery(con, "PRAGMA table_info(reports)")$name)
  expect_true(DBI::dbExistsTable(con, "report_files"))
  r <- db_reports(con)
  expect_equal(nrow(r), 1)
  expect_true(is.na(r$comment))
  expect_false(r$has_file)
})
