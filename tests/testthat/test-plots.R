test_that("plot functions return plotly objects with and without data", {
  con <- db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  expect_s3_class(plot_ion_metric(db_ions(con), "error_ppm"), "plotly")
  expect_s3_class(plot_status_timeline(db_sections(con)), "plotly")

  import_folder(testthat::test_path("fixtures"), con = con)
  ions <- db_ions(con, "Positive")
  for (p in list(
    plot_ion_metric(ions, "error_ppm", limits = "band"),
    plot_ion_metric(ions, "resolution", limits = "min"),
    plot_ion_metric(ions, "sum_intensity", limits = "min", log_y = TRUE),
    plot_zeno_ratio(ions),
    plot_calibration(db_sections(con, "Positive")),
    plot_alignment(db_alignment(con, "Positive")),
    plot_status_timeline(db_sections(con))
  )) {
    expect_s3_class(p, "plotly")
    expect_no_error(plotly::plotly_build(p))
  }
})

test_that("messages are escaped and shown in hover text and timeline", {
  con <- db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  f <- testthat::test_path("fixtures", "pos_quickcheck.xps")
  db_insert_report(con, parse_quickcheck(f), comment = "Tuned <b>MS</b> & cleaned")

  b <- plotly::plotly_build(plot_ion_metric(db_ions(con), "error_ppm", limits = "band"))
  hover <- unlist(lapply(b$x$data, `[[`, "text"))
  expect_true(any(grepl("Note:</i> Tuned &lt;b&gt;MS&lt;/b&gt; &amp; cleaned", hover, fixed = TRUE)))
  expect_false(any(grepl("<b>MS</b>", hover, fixed = TRUE)))

  tl <- plotly::plotly_build(plot_status_timeline(db_sections(con)))
  names <- vapply(tl$x$data, function(t) t$name %||% "", "")
  expect_true("Message" %in% names)
})

test_that("hover text stays an array for single-point traces", {
  # plotly.js only fills %{text} from a per-point array; a length-1 vector
  # must not be sent as a scalar string
  con <- db_connect(tempfile(fileext = ".sqlite"))
  on.exit(DBI::dbDisconnect(con))
  f <- testthat::test_path("fixtures", "pos_quickcheck.xps")
  db_insert_report(con, parse_quickcheck(f), comment = "Single note")

  tl <- plotly::plotly_build(plot_status_timeline(db_sections(con)))
  msg <- Filter(function(t) identical(t$name, "Message"), tl$x$data)[[1]]
  expect_length(msg$text, 1)
  expect_s3_class(msg$text, "AsIs")
  expect_match(msg$text, "Single note", fixed = TRUE)

  b <- plotly::plotly_build(plot_ion_metric(db_ions(con), "error_ppm", limits = "band"))
  series <- Filter(function(t) identical(t$mode, "lines+markers"), b$x$data)
  expect_gt(length(series), 0)
  for (t in series) expect_s3_class(t$text, "AsIs")
})
