#' select_view UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @import shinyWidgets
#' @inerit
mod_survey_input_user_ui <- function(id){
  ns <- NS(id)
  survey <- config::get(file = "conf/survey_input_config.yml")[[golem::get_golem_options("annotator_config")]]
  tagList(
    purrr::map(
      survey,
      function(x){
        choices <- x$input_choices %>% 
          base::unlist() %>%
          setNames(NULL)
        prompt <- x$prompt
        initial_choice <- x$input_choices$initial_choice
        colname <- x$colname
        
        #' set button
        radioGroupButtons(
          size = 'sm',
          inputId = ns(colname),
          selected = initial_choice,
          label = h4(prompt),
          choiceNames = choices,
          choiceValues = choices,
          checkIcon = list(
            yes = icon("ok", 
                       lib = "glyphicon"))
        )
      }
    )
  )
}
    
#' select_view Server Function
#' @noRd 
mod_survey_input_user_server <- function(input, output, session, values){
  ns <- session$ns
  survey <- config::get(file = "conf/survey_input_config.yml")[["psorcast_plaque"]]
  
  #' change this  
  status_vec <- purrr::map(
    survey, function(x){
    x$colname}) %>% 
    purrr::reduce(., c)
  
  #' change input
  purrr::walk(status_vec, function(status){
    observeEvent(input[[status]], {
      values$userInput[[status]] <- input[[status]]
    })
  })
}
    
## To be copied in the UI
# mod_select_view_ui("select_view_ui_1")
    
## To be copied in the server
# callModule(mod_select_view_server, "select_view_ui_1")
 
