parse_user_input <- function(data, 
                             user_input,
                             cols,
                             uid,
                             curr_index,
                             keep_metadata){
  tryCatch({
    purrr::walk(user_input, function(user_input){
      if(user_input == "None Selected"){
        stop("")
      }
    })
    purrr::map(cols, function(col){
      data %>% 
        dplyr::mutate(
          !!sym(col) := ifelse(!!sym(uid) == curr_index, 
                               user_input[[col]], 
                               !!sym(col))) %>%
        dplyr::select(all_of(keep_metadata), col, imagePath)
    }) %>% 
      purrr::reduce(dplyr::full_join) %>%
      dplyr::mutate(annotationTimestamp = lubridate::now())
  }, error = function(e){
    data
  })
}


#' store_input Server Function
#'
#' @noRd 
survey_input_store <- function(data, 
                               index,
                               user_input,
                               survey_colnames){
  read_synapse_config <- config::get(file = "conf/synapse_input_config.yml")[[golem::get_golem_options("annotator_config")]]
  read_survey_config <- config::get(file = "conf/survey_input_config.yml")[[golem::get_golem_options("annotator_config")]]
  keep_metadata <- read_synapse_config$keep_metadata %>% 
    unlist() %>% 
    purrr::set_names(NULL)
  uid <- read_synapse_config$unique_identifier
  curr_index <- data$recordId[index]
  status_vec <- purrr::map(
    read_survey_config, function(survey){
      colname <- survey$colname
      colname
    }) %>% 
    purrr::reduce(., c)
  data %>% 
    parse_user_input(
      data = .,
      user_input = user_input,
      cols = status_vec,
      uid = uid,
      curr_index = curr_index,
      keep_metadata = keep_metadata)
}
    
## To be copied in the UI
# mod_store_input_ui("store_input_ui_1")
    
## To be copied in the server
# callModule(mod_store_input_server, "store_input_ui_1")
 
