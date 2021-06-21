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
    synapse_config$filehandle_cols <- config$synapse_opts$column_target
    synapse_config$synapse_tbl_id <- config$synapse_opts$synID$tbl
    synapse_config$n_batch <- config$synapse_opts$n_batch
    synapse_config$uid <- config$synapse_opts$unique_identifier
    synapse_config$keep_metadata <- config$synapse_opts$keep_metadata
    synapse_config$output_parent_id <- config$synapse_opts$synID$parent_output
    synapse_config$output_filename <- config$synapse_opts$output_filename
    synapse_config$sort_keys <- config$synapse_opts$sort_keys
    
    #' return list of values
    return(synapse_config)
}

parse_image_opts <- function(config){
    image_config <- list()
    image_config$width <- config$image_opts$width
    image_config$height <- config$image_opts$height
    return(image_config)
}


create_user_directory <- function(user_dir_parent, curr_annotator){
    #' create user directory
    dir.create(user_dir_parent) 
    user_dir <- file.path(user_dir_parent, curr_annotator)
    dir.create(file.path(user_dir), showWarnings = FALSE) 
    dir.create(file.path(user_dir, "downloaded_files"), showWarnings = FALSE)
    dir.create(file.path(user_dir, "processed_files"), showWarnings = FALSE)
    return(user_dir)
}


clear_directory <- function(user_dir, annotator){
    unlink(glue::glue(
        "{user_dir}/{annotator}/downloaded_files/*"), 
        recursive = T, force = T)
    unlink(glue::glue(
        "{user_dir}/{annotator}/processed_files/*"), 
        recursive = T, force = T)
}

parse_initial_selection <- function(input){
    if(is.null(input)){
        character(0)
    }else{
        input
    }
}

