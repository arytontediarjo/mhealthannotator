#' function to parse initial selection
#' place null if user is not giving any input
#' @param input user input
parse_initial_selection <- function(input){
    if(is.null(input)){
        character(0)
    }else{
        input
    }
}


#' Function to parse selection (single/multiple)
#' @param row_input input from user
#' @param selected previously selected
#' @return the selected shiny user input
parse_select_value <- function(row_input, selected){
    if(is.na(row_input)){
        parse_initial_selection(selected)
    }else{
        return(
            stringr::str_split(row_input, ",") %>% 
                unlist(.) %>% 
                stringr::str_trim(.))
    }
}

#' Function to parse slider (single/multiple)
#' @param row_input input from user
#' @param selected previously selected
#' @return the selected shiny user input
parse_slider_value <- function(row_input, selected){
    if(is.na(row_input)){
        return(selected)
    }else{
        return(as.numeric(row_input))
    }
}


#' Function to update user input shiny buttons by
#' maintaining previously selected buttons and/or
#' updating it with new ones
#' 
#' @param reactive_values access reactive values
#' @param session access shiny session
#' @param curr_index access current index
#' @param config access annotator config file
#' @return updated dataframe with input
update_inputs <- function(reactive_values,
                           session, 
                           curr_index,
                           config){
    select_view_ns <- NS("survey_input_ui")
    purrr::walk(config, function(survey){
        selected <- survey$selected
        colname <- survey$colname
        type <- survey$type
        row_input <- reactive_values$useDf[[colname]][curr_index]
        if(type == "radio"){
            updateRadioGroupButtons(session, 
                                    select_view_ns(colname), 
                                    selected = parse_select_value(
                                        row_input, selected = selected))
        }else if(type == "select"){
            updatePickerInput(session, 
                              select_view_ns(colname), 
                              selected = parse_select_value(
                                  row_input, selected = selected))
        }else if(type == "slider"){
            updateSliderTextInput(session, 
                                  select_view_ns(colname), 
                                  selected = as.numeric(parse_slider_value(
                                  row_input, selected = selected)))
        }else{
            updateCheckboxGroupButtons(session, 
                                       select_view_ns(colname), 
                                       selected = parse_select_value(
                                           row_input, selected = selected))
        }
    })
    return(reactive_values)
}