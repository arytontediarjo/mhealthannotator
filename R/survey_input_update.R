parse_select_value <- function(row_input){
    if(is.na(row_input)){
        return(character(0))
    }else{
        return(
            stringr::str_split(row_input, ",") %>% 
                unlist(.) %>% 
                stringr::str_trim(.))
    }
}

parse_slider_value <- function(row_input){
    if(is.na(row_input)){
        return("None Selected")
    }else{
        return(as.numeric(row_input))
    }
}

update_buttons <- function(reactive_values,
                           session, 
                           curr_index,
                           survey_colnames,
                           survey_types){
    select_view_ns <- NS("ui_1")
    purrr::walk2(survey_colnames, survey_types, 
                function(survey_colname, survey_type){
        row_input <- reactive_values$useDf[[survey_colname]][curr_index]
        if(is.na(row_input)){
            reactive_values$userInput[[survey_colname]] <- "None Selected"
        }else{
            reactive_values$userInput[[survey_colname]] <- row_input
        }
        if(survey_type == "radio"){
            updateRadioGroupButtons(session, 
                                    select_view_ns(survey_colname), 
                                    selected = parse_select_value(row_input))
        }else if(survey_type == "select"){
            updatePickerInput(session, 
                              select_view_ns(survey_colname), 
                              selected = parse_select_value(row_input))
        }else{
            updateSliderTextInput(session, 
                              select_view_ns(survey_colname), 
                              selected = as.numeric(parse_slider_value(row_input)))
        }
    })
    return(reactive_values)
}