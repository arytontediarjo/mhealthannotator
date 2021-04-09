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
    purrr::map(survey_colnames, function(col){
      curr_uid <- data[[uid]][curr_index]
      data %>% 
        dplyr::mutate(
          !!sym(col) := ifelse(!!sym(uid) == curr_uid, 
                               user_inputs[[col]], 
                               !!sym(col))) %>%
        dplyr::select(all_of(uid), all_of(keep_metadata), col, imagePath)}) %>% 
      purrr::reduce(dplyr::full_join) %>%
      dplyr::mutate(annotationTimestamp = lubridate::now())
  }, error = function(e){
    print(e)
    data
  })
}
    
## To be copied in the UI
# mod_store_input_ui("store_input_ui_1")
    
## To be copied in the server
# callModule(mod_store_input_server, "store_input_ui_1")
 
