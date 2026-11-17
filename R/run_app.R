#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts, e.g. `db_path = "path/to/zenoqc.sqlite"`
#' to use another database than the configured one (see [db_path()]).
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
	onStart = NULL,
	options = list(),
	enableBookmarking = NULL,
	uiPattern = "/",
	...
) {
	with_golem_options(
		app = shinyApp(
			ui = app_ui,
			server = app_server,
			onStart = onStart,
			options = options,
			enableBookmarking = enableBookmarking,
			uiPattern = uiPattern
		),
		golem_opts = list(...)
	)
}
