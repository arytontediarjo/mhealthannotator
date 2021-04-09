update_buttons <- function(session, 
                           data, 
                           curr_index,
                           survey_config){
    select_view_ns <- NS("ui_1")
    purrr::map(survey_config, function(survey){
        if(survey$type == "radio"){
            updateRadioGroupButtons(session, 
                                    select_view_ns(survey$colname), 
                                    selected = ifelse(
                                        is.na(data[[survey$colname]][curr_index]), 
                                        "None Selected", 
                                        data[[survey$colname]][curr_index]))
        }else if(survey$type == "select"){
            updatePickerInput(session, 
                              select_view_ns(survey$colname), 
                              selected = character(0))
        }else{
            stop("")
        }
    })
}