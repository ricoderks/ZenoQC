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

test_that("the navbar has an About link after History", {
  html <- as.character(app_ui(NULL))
  expect_match(html, 'id="about-open"', fixed = TRUE)
  expect_lt(regexpr("History", html), regexpr('id="about-open"', html))
})

test_that("the About popup explains the app and links GitHub", {
  html <- as.character(about_modal("/x/zenoqc.sqlite"))
  expect_match(html, "About ZenoQC", fixed = TRUE)
  expect_match(html, 'href="https://github.com/ricoderks/ZenoQC"', fixed = TRUE)
  expect_match(html, 'href="https://github.com/ricoderks/ZenoQC/issues"', fixed = TRUE)
  expect_match(html, 'target="_blank"', fixed = TRUE)
  expect_match(html, app_version(), fixed = TRUE)
  expect_match(html, "/x/zenoqc.sqlite", fixed = TRUE)
})

test_that("clicking About opens the popup", {
  shiny::testServer(mod_about_server, args = list(db_path = "x.sqlite"), {
    expect_no_error(session$setInputs(open = 1))
  })
})

test_that("the Period dropdown is attached to the body so cards can't clip it", {
  html <- as.character(period_filter_ui(NS("pos")))
  expect_match(html, '"dropdownParent":"body"', fixed = TRUE)
})
