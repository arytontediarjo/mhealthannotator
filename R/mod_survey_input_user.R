#' @title UI Function for creating survey buttons
#'
#' @description Gives functionality to creating the survey buttons UI,
#' populating with menu that can run different kinds of survey buttons
#' based on different input types 
#' 
#' @export
#' 
#' @rdname survey_input_user_ui
#' 
#' @import shinyWidgets
#' 
#' @param id the id
#'
#' @return the UI for survey inputs in ShinyApp
mod_survey_input_user_ui <- function(id){
  ns <- NS(id)
  
  #' read config file
  config_path <- file.path(golem::get_golem_options("config"))
  survey_config <- config::get(file = config_path) %>% .$survey_opts
  check_survey_config(survey_config)
  
  #' create based on config
  tagList(
    purrr::map(
      survey_config,
      function(x){
        choices <- x$input_choices
        prompt <- x$prompt
        colname <- x$colname
        selected <- parse_initial_selection(x$selected)
        buttonType <- x$type
        
        if(buttonType == "radio"){
          #' set button
          radioGroupButtons(
            size = 'normal',
            inputId = ns(colname),
            selected = selected,
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
    
#' @title Server Function for creating survey buttons
#'
#' @description Gives functionality to creating the survey buttons server-side,
#' helps parse through each survey columns requirement and render buttons
#' according to user configuration. 
#' Designed to take multiple input as 'comma-separated'
#' 
#' @export
#' 
#' @rdname survey_input_user_server
#' 
#' @param input the input variables from [shiny::callModule()]
#' @param output the output variables from [shiny::callModule()]
#' @param session the session from [shiny::callModule()]
#' @param values the reactive values from server
#' @param survey_colnames the column used to store survey
#' 
#' @return the UI for survey inputs in ShinyApp
mod_survey_input_user_server <- function(input, output, session, survey_colnames, values){
  ns <- session$ns
  
  #' change input
  purrr::walk(survey_colnames, function(col){
    observeEvent(input[[col]], {
      if(is.null(input[[col]])){
        values$userInput[[col]] <- ""
      }else{
        values$userInput[[col]] <- glue::glue_collapse(
          input[[col]], sep = ", ")
      }
    }, ignoreNULL = FALSE)
  })
}
