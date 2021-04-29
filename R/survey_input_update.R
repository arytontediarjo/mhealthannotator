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

parse_slider_value <- function(row_input, selected){
    if(is.na(row_input)){
        return(selected)
    }else{
        return(as.numeric(row_input))
    }
}

update_buttons <- function(reactive_values,
                           session, 
                           curr_index,
                           survey_config){
    select_view_ns <- NS("ui_1")
    purrr::walk(survey_config, function(survey){
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