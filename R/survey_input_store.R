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
    purrr::map(survey_colnames, function(col){
      data %>% 
        dplyr::mutate(
          !!sym(col) := ifelse(!!sym(uid) == curr_uid, 
                               user_inputs[[col]], 
                               !!sym(col))) %>%
        dplyr::select(all_of(uid), all_of(keep_metadata), col, imagePath, annotationTimestamp)}) %>% 
      purrr::reduce(dplyr::full_join) %>%
      dplyr::mutate(
        annotationTimestamp = ifelse(!!sym(uid) == curr_uid, 
                                     as.character(lubridate::now()), 
                                     annotationTimestamp)
      )
  }, error = function(e){
    print(e)
    data
  })
}
    
## To be copied in the UI
# mod_store_input_ui("store_input_ui_1")
    
## To be copied in the server
# callModule(mod_store_input_server, "store_input_ui_1")
 
