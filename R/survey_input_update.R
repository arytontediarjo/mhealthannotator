update_buttons <- function(session, values){
    select_view_ns <- NS("ui_1")
    survey <- config::get(file = "conf/survey_input_config.yml")[[golem::get_golem_options("annotator_config")]]
    purrr::walk(survey, function(col){
        colname <- col$colname
        initial_choice <- col$input_choices$initial_choice
        updateRadioGroupButtons(session, 
                                select_view_ns(colname), 
                                selected = ifelse(
                                    is.na(values$useDf[[colname]][values$ii]), 
                                    initial_choice, 
                                    values$useDf[[colname]][values$ii]))
    })
}