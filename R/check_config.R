check_synapse_config <- function(config){
    #'  check synapse config
    tryCatch({
        if(class(config$synapse_tbl_id) != "character"){
            stop("Config Error (synapse table id): parse in valid synapse ID")
        } 
        else if(class(config$output_filename) != "character"){
            stop("Config Error (output filename): parse in character")
        } 
        else if(class(config$filehandle_cols) != "character"){
            stop("Config Error (filehandle_cols): parse in character")
        } 
        else if(class(config$uid) != "character"){
            stop("Config Error (uid): parse in character")
        } 
        else if(class(config$n_batch) != "integer"){
            stop("Config Error (n_batch): parse in integer")
        } 
        else if(class(config$uid) != "character"){
            stop("Config Error (uid): parse in character")
        }else{
            return(config)
        }
    }, error = function(e){
        stop(e$message)
    })
}

#' restrict survey type to several
check_survey_config <- function(config){
    available_button_types <- c("radio", 
                                "slider",
                                "checkbox_group")
    button_all_available <- config %>% 
        purrr::map(~.x$type) %>% 
        purrr::reduce(c) %in% 
        available_button_types %>%
        all()
    if(!button_all_available){
        stop("button type not available. Please parse in option of `radio`, `slider`, `checkbox-group`")
    }else{
        return(config)
    }
}
