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
