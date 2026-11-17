fixture <- function(x) testthat::test_path("fixtures", x)

test_that("text tokens join split glyph runs (minus signs)", {
  tok <- xps_text_tokens(fixture("pos_quickcheck.xps"))
  expect_true("Error (ppm)" %in% tok)
  expect_true("-2.00" %in% tok)
  expect_true(any(grepl("^Delay: -12\\.3405377523136, previous was -11\\.9729793440288", tok)))
  expect_equal(sum(tok == "#PAGEBREAK"), 2)
})

test_that("positive report is parsed", {
  p <- parse_quickcheck(fixture("pos_quickcheck.xps"))
  r <- p$report
  expect_equal(r$polarity, "Positive")
  expect_equal(r$acquired_at, "2026-11-17 05:39:00")
  expect_equal(r$serial_number, "FO20062501")
  expect_equal(r$instrument_model, "ZenoTOF 7600+ System")
  expect_equal(r$overall_status, "Pass")
  expect_equal(r$align_channel, 2L)

  expect_equal(p$sections$section, c("Achieve Stable Spray / Modify", "Channel Alignment",
                                     "TOF MS Mass Check", "TOF MS/MS Mass Check",
                                     "TOF MS/MS Mass Check Zeno On"))
  expect_true(all(p$sections$status == "Pass"))
  ms <- p$sections[p$sections$section == "TOF MS Mass Check", ]
  expect_equal(ms$slope, 0.000490018723729782)
  expect_equal(ms$delay, -12.3405377523136)
  expect_equal(ms$delay_prev, -11.9729793440288)
  expect_equal(ms$n_spectra, 10L)

  ions <- p$ions
  expect_equal(nrow(ions), 50)
  tofms <- ions[ions$section == "TOF MS Mass Check", ]
  expect_equal(sort(unique(tofms$target_mz)), c(132.9049, 829.5393))
  get <- function(sec, mz, metric) {
    ions$actual[ions$section == sec & ions$target_mz == mz & ions$metric == metric]
  }
  expect_equal(get("TOF MS Mass Check", 132.9049, "resolution"), 22130)
  expect_equal(get("TOF MS Mass Check", 132.9049, "error_ppm"), -1.51455)
  expect_equal(get("TOF MS/MS Mass Check", 494.3337, "sum_intensity"), 2.2635e5)
  expect_equal(get("TOF MS/MS Mass Check Zeno On", 298.2125, "error_ppm"), 1.3102)
  expect_equal(sort(unique(ions$target_mz[ions$section == "TOF MS/MS Mass Check Zeno On"])),
               c(185.1285, 298.2125, 494.3337, 607.4178))
  expect_equal(ions$min[ions$section == "TOF MS Mass Check" & ions$metric == "error_ppm"],
               c(-2, -2))
})

test_that("negative report is parsed", {
  p <- parse_quickcheck(fixture("neg_quickcheck.xps"))
  expect_equal(p$report$polarity, "Negative")
  expect_equal(p$report$acquired_at, "2026-11-05 07:59:00")
  expect_equal(sort(unique(p$ions$target_mz[p$ions$section == "TOF MS/MS Mass Check"])),
               c(248.9604, 384.9352, 520.91, 656.8848))
  a <- p$alignment
  expect_equal(nrow(a), 6)
  ch1 <- a[a$channel == 1 & a$parameter == "intercept", ]
  expect_equal(ch1$actual, -16.133777288)
  expect_equal(ch1$initial, -15.776036078)
  expect_equal(ch1$min, -50)
})

test_that("invalid files give a clear error", {
  bad <- tempfile(fileext = ".xps")
  writeLines("not an xps file", bad)
  expect_error(parse_quickcheck(bad), "not a valid XPS file")
})

test_that("all example reports in Docs/xps parse", {
  dir <- testthat::test_path("..", "..", "Docs", "xps")
  skip_if_not(dir.exists(dir), "example reports not available")
  files <- list.files(dir, pattern = "\\.xps$", full.names = TRUE)
  for (f in files) {
    p <- expect_no_error(parse_quickcheck(f))
    expect_gte(nrow(p$ions), 50)
    expect_equal(nrow(p$alignment), 6)
  }
})
