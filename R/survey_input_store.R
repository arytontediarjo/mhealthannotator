#' store_input Server Function
#'
#' @noRd 
survey_input_store <- function(data, 
                               curr_index,
                               user_inputs,
                               survey_colnames,
                               keep_metadata,
                               uid){
  tryCatch({
    curr_uid <- data[[uid]][curr_index]
    curr_fileColumnName <- data$fileColumnName[curr_index]
    user_input_data <- user_inputs %>%
      tibble::enframe(.) %>% 
      dplyr::mutate(value = unlist(value)) %>%
      tidyr::pivot_wider(name) %>%
      dplyr::mutate(
        !!sym(uid) := curr_uid,
          fileColumnName = curr_fileColumnName,
          annotationTimestamp = as.character(lubridate::now()))
    data %>% 
      dplyr::rows_update(user_input_data, by = c(uid, "fileColumnName")) %>%
      dplyr::select(all_of(uid), 
                    all_of(keep_metadata), 
                    all_of(survey_colnames),
                    fileColumnName, 
                    imagePath, 
                    annotationTimestamp)
  }, error = function(e){data})
}
    
## To be copied in the UI
# mod_store_input_ui("store_input_ui_1")
    
## To be copied in the server
# callModule(mod_store_input_server, "store_input_ui_1")
 
