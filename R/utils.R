#' Function to parse unique id for iteration
#' THIS IS MODULAR
parse_uid_to_string <- function(uid){
    uid %>% 
        purrr::map_chr(., function(x){glue::glue("'", x, "'")}) %>%
        paste0(., collapse = ",") %>%
        glue::glue("(", ., ")")
}

parse_survey_opts <- function(config){
    #' instantiate survey configuration
    survey_config <- list()
    
    #' read survey configuratation
    survey_config$survey_colnames <- config$survey_opts %>% 
        purrr::map(function(survey){survey$colname}) %>% 
        base::unlist() %>% 
        purrr::set_names(NULL)
    
    #' read survey configuration
    survey_config$button_types <- config$survey_opts %>% 
        purrr::map(function(survey){survey$type}) %>% 
        base::unlist() %>% 
        purrr::set_names(NULL)
    return(survey_config)
}

parse_synapse_opts <- function(config){
    #' instantiate empty list
    synapse_config <- list()    
    
    #' read synapse configuratiton 
    synapse_config$filehandle_cols <- config$synapse_opts$column_target %>% 
        unlist(.) %>% setNames(NULL)
    synapse_config$synapse_tbl <- config$synapse_opts$synID$tbl
    synapse_config$n_batch <- config$synapse_opts$n_batch
    synapse_config$uid <- config$synapse_opts$unique_identifier
    synapse_config$keep_metadata <- config$synapse_opts$keep_metadata %>% unlist(.) %>% setNames(NULL)
    synapse_config$output_parent_id <- config$synapse_opts$synID$parent_output
    synapse_config$output_filename <- config$synapse_opts$output_filename
    
    #' return list of values
    return(synapse_config)
}
