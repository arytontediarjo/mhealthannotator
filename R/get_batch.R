#' Process Synapse Table Column Files
#' 
#' Process Synapse Table Filehandles in batch processing,
#' will take unannotated data and batch process each 
#' of them based on a visualization function
#' 
#' @param syn synapseclient
#' @param all_data dataframe of whole data from synapse table
#' @param curated_data dataframe of annotated ata
#' @param synapse_tbl_id synapse source table id
#' @param filehandle_cols filehandle column target to parse
#' @param uid unique identifier of each files
#' @param keep_metadata metadata to keep from the table
#' @param n_batch number of batch per session
#' @param output_location where to store processed files
#' @param cache_location where to find raw files
#' @param visualization_funs function to visualize data
#' @param survey_colnames the column for storing survey input
#' @param sort_keys sorting keys
#' 
#' @import tibble
#' @import magrittr
#' @import dplyr
#' @import purrr
#' @import tidyr
#' @import glue
#' 
#' @return a dataframe containing processed 
#' Table column files that will be used
#' for rendering in the Shiny App
#' 
#' @export
get_batch <- function(syn, all_data, curated_data, 
                      synapse_tbl_id, filehandle_cols, 
                      uid, survey_colnames,
                      keep_metadata, n_batch,
                      output_location, cache_location,
                      visualization_funs,
                      sort_keys = NULL){
  
  # check sorting
  if(is.null(sort_keys)){
    sort_keys <- uid
  }
  
  # get unannotated data and corresponding filehandleids
  all_data %>%
    dplyr::anti_join(
      curated_data,
      by = c(uid, "fileColumnName")) %>%
    
    # get unannotated files from synapse
    get_session_images(
      syn = syn,
      data = .,
      uid = uid,
      synapse_tbl_id = synapse_tbl_id,
      filehandle_cols = filehandle_cols,
      keep_metadata = keep_metadata,
      n_batch = n_batch,
      cache_location = cache_location) %>% 
    
    # visualize data
    visualize_column_files(
      funs = visualization_funs,
      output_location = output_location) %>%
    
    # clean data by converting all to character
    # drop NA, add survey columns, and several other
    # metadata
    dplyr::mutate_all(as.character) %>%
    tidyr::drop_na(any_of(c("imagePath"))) %>%
    dplyr::bind_cols(
      (survey_tbl <- purrr::map_dfc(
        survey_colnames, function(x){
        tibble(!!sym(x) := as.character(NA))
      }))) %>%
    dplyr::mutate(annotationTimestamp = NA_character_) %>%
    dplyr::arrange(!!sym(sort_keys))
}