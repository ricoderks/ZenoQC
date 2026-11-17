# Launch the ShinyApp (for Posit Connect / Shiny Server / RStudio "Run App")
pkgload::load_all(export_all = FALSE, helpers = FALSE, attach_testthat = FALSE)
options("golem.app.prod" = TRUE)
zenoQC::run_app()
