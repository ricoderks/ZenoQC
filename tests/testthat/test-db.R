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
