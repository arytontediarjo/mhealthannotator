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
        choices <- x$input_choices
        prompt <- x$prompt
        colname <- x$colname
        selected <- x$selected
        buttonType <- x$type
        
        if(buttonType == "radio"){
          #' set button
          radioGroupButtons(
            size = 'sm',
            inputId = ns(colname),
            selected = character(0),
            label = h4(prompt),
            choices = choices,
            checkIcon = list(
              yes = icon("ok", lib = "glyphicon")))
        }else if(buttonType == "select"){
          pickerInput(ns(colname),
                      h4(prompt), 
                      choices = choices, 
                      options = list(`actions-box` = TRUE,
                                     `none-selected-text` = "None Selected"), 
                      multiple = TRUE)
        }else if(buttonType == "slider"){
          sliderTextInput(
            ns(colname),
            h4(prompt), 
            selected = "None Selected",
            choices = c(
              "None Selected",
              seq(x$input_choices$choice_min, 
                  x$input_choices$choice_max, 
                  x$input_choices$increments)),
            grid = TRUE,
            force_edges = TRUE
          )
        }else if(buttonType == "checkbox_group"){
          checkboxGroupButtons(
            inputId = ns(colname),
            label = h4(prompt), 
            selected = selected,
            choices = choices,
            checkIcon = list(
              yes = icon("ok", lib = "glyphicon")))
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
      if(is.null(input[[status]])){
        values$userInput[[status]] <- ""
      }else{
        values$userInput[[status]] <- glue::glue_collapse(
          input[[status]], sep = ", ")
      }
    }, ignoreNULL = FALSE)
  })
}
    
## To be copied in the UI
# mod_select_view_ui("select_view_ui_1")
    
## To be copied in the server
# callModule(mod_select_view_server, "select_view_ui_1")
 
