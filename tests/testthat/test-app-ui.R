test_that("the UI shows the app version and links the app icon", {
  rendered <- htmltools::renderTags(app_ui(NULL))
  html <- paste(rendered$head, rendered$html)
  v <- as.character(utils::packageVersion("zenoQC"))
  expect_match(html, paste0("v", v), fixed = TRUE)
  expect_match(html, sprintf("ZenoQC v%s", v), fixed = TRUE)
  expect_match(html, "www/favicon.svg", fixed = TRUE)
  expect_match(html, "www/favicon.ico", fixed = TRUE)
  expect_match(html, "www/apple-touch-icon.png", fixed = TRUE)
})

test_that("the icon files are installed with the app", {
  for (f in c("favicon.svg", "favicon.ico", "apple-touch-icon.png")) {
    expect_true(file.exists(app_sys("app", "www", f)), info = f)
  }
})

test_that("the page has a single, versioned window title", {
  rendered <- htmltools::renderTags(app_ui(NULL))
  html <- paste(rendered$head, rendered$html)
  titles <- regmatches(html, gregexpr("<title>[^<]*</title>", html))[[1]]
  expect_true(length(titles) >= 1)
  expect_true(all(titles == sprintf("<title>%s</title>", app_title())))
})
