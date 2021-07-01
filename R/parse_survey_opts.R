#' function to parse survey question from the config file
#' @param config annotator config file being used 
#' @return named list of survey question and its options
parse_survey_opts <- function(config){
    #' instantiate survey configuration
    survey_config <- list()
    
    #' read survey configuratation
    survey_config$survey_colnames <- config %>% 
        purrr::map(function(survey){survey$colname}) %>% 
        base::unlist() %>% 
        purrr::set_names(NULL)
    
    #' read survey configuration
    survey_config$button_types <- config %>% 
        purrr::map(function(survey){survey$type}) %>% 
        base::unlist() %>% 
        purrr::set_names(NULL)
    return(survey_config)
}