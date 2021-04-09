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
  
  #' read config file
  config_path <- file.path("conf", golem::get_golem_options("annotator_config"))
  survey_config <- config::get(file = config_path) %>% .$survey_opts
  
  #' create based on config
  tagList(
    purrr::map(
      survey_config,
      function(x){
        choices <- x$input_choices %>% 
          base::unlist() %>%
          setNames(NULL)
        prompt <- x$prompt
        colname <- x$colname
        buttonType <- x$type
        
        if(buttonType == "radio"){
          #' set button
          radioGroupButtons(
            size = 'sm',
            inputId = ns(colname),
            selected = "None Selected",
            label = h4(prompt),
            choiceNames = c("None Selected", choices),
            choiceValues = c("None Selected", choices),
            checkIcon = list(
              yes = icon("ok", lib = "glyphicon")))
        }else if(buttonType == "select"){
          pickerInput(ns(colname),
                      h4(prompt), 
                      choices = choices, 
                      options = list(`actions-box` = TRUE), 
                      multiple = TRUE)
        }else if(buttonType == "slider"){
          sliderTextInput(
            ns(colname),
            h4(prompt), 
            choices = c(
              seq(x$input_choices$choice_min, 
                  x$input_choices$choice_max, 
                  x$input_choices$increments)),
            grid = TRUE,
            force_edges = TRUE
          )
        }else{
          stop("Please parse in button UI")
        }
      }
    )
  )
}
    
#' select_view Server Function
#' @noRd 
mod_survey_input_user_server <- function(input, output, session, values){
  ns <- session$ns
  
  config_path <- file.path("conf", golem::get_golem_options("annotator_config"))
  survey_config <- config::get(file = config_path) %>% .$survey_opts
  
  #' change this  
  status_vec <- purrr::map(
    survey_config, function(x){
    x$colname}) %>% 
    purrr::reduce(., c)
  
  #' change input
  purrr::walk(status_vec, function(status){
    observeEvent(input[[status]], {
      values$userInput[[status]] <- glue::glue_collapse(input[[status]], sep = ", ")
    })
  })
}
    
## To be copied in the UI
# mod_select_view_ui("select_view_ui_1")
    
## To be copied in the server
# callModule(mod_select_view_server, "select_view_ui_1")
 
