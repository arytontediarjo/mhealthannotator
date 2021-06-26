#' function to parse survey question from the config file
#' @param config annotator config file being used 
#' @return named list of survey question and its options
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


#' function to parse synapse options
#' @param config config
#' @return named list of synapse options being used
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


#' Function to parse image options
#' @param config config
#' @return named list of image options being used
parse_image_opts <- function(config){
    image_config <- list()
    image_config$width <- config$image_opts$width
    image_config$height <- config$image_opts$height
    return(image_config)
}

#' Function to create user directory for temporarily
#' storing image files (to not overpopulate Shiny Server)
#' @param curr_annotator user name of the annotator
create_user_directory <- function(curr_annotator){
    #' create user directory
    user_dir <- file.path(
        "dir", curr_annotator)
    dir.create(
        user_dir, showWarnings = FALSE) 
    dir.create(
        file.path(user_dir, "downloaded_files"), 
        showWarnings = FALSE)
    dir.create(
        file.path(user_dir, "processed_files"), 
        showWarnings = FALSE)
}

#' Function to clear user directory
#' storing image files (to not overpopulate Shiny Server)
#' @param curr_annotator user name of the annotator
clear_directory <- function(curr_annotator){
    unlink(glue::glue(
        "dir/{curr_annotator}"), 
        recursive = T, force = T)
    unlink(glue::glue(
        "dir/{curr_annotator}"), 
        recursive = T, force = T)
}

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

