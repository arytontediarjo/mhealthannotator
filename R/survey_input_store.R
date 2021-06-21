#' Function to store survey inputs from shiny app
#' 
#' @param data data of annotations
#' @param curr_index current index marker
#' @param user_inputs input given by users
#' @param survey_colnames columns used for the surveys
#' @param keep_metadata what metadata to keep
#' @param uid what are the unique ids (recordId, healthcode, participantId etc.)
#'
#' @export
#'
#' @return dataframe of annotation based on current index
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
