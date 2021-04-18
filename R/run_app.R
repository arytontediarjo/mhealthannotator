#' Run the Shiny Application
#'
#' @param ... A series of options to be used inside the app.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(annotator_config = "psorcast_plaque", 
                    visual_funs = visualize_photo) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(
      annotator_config = annotator_config,
      visual_funs = visual_funs
    )
  )
}
