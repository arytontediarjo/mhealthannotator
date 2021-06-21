#' Get Table Unique Identifier String Filter
#' 
#' This is a helper function to build string filter (SQL-like)
#' for filtering Synapse Table before downloading files 
#' 
#' Note: This is done to enable small-batch download
#' 
#' @param uid unique identifier used to 
#' @return a string of unique identifier that will
#' be included in the batch with parentheses 
get_table_string_filters <- function(uid){
  uid %>% 
    purrr::map_chr(., function(x){glue::glue("'", x, "'")}) %>%
    paste0(., collapse = ",") %>%
    glue::glue("(", ., ")")
}


#' Get unannotated files in batch
#' 
#' Helper function for downloading a number of Synapse table-attached 
#' files according to input batch, and process using desired function
#' 
#' @param data containing un-annotated data based on each user
#' @inheritParams batch_process_filehandles
get_unannotated_files_in_batch <- function(syn, data, synapse_tbl_id, 
                                           filehandle_cols, uid, 
                                           keep_metadata, 
                                           n_batch, output_location, 
                                           cache_location, visualization_funs){
  
  # set cache location
  syn$cache$cache_root_dir <- cache_location
  
  # get sql string statement for filtering data in synapse table
  get_subset <- data %>%
    dplyr::slice(1:n_batch) %>%
    .[[uid]] %>% 
    get_table_string_filters()
  
  # get synapse table entity
  entity <- syn$tableQuery(
    glue::glue(
      "SELECT * FROM {synapse_tbl_id} WHERE recordId IN {get_subset}"))
  
  # download all table columns
  result <- syn$downloadTableColumns(
    table = entity, 
    columns = filehandle_cols) %>%
    tibble::enframe(.) %>%
    tidyr::unnest(value) %>%
    dplyr::select(
      fileHandleId = name, 
      filePath = value) %>%
    dplyr::mutate(filePath = unlist(filePath)) %>%
    dplyr::inner_join(data, by = c("fileHandleId")) %>%
    dplyr::select(all_of(uid), all_of(keep_metadata), 
                  fileColumnName, filePath)
}

#' Visualize Synapse Table Column Files
#' 
#' Helper function to visualize synapse column files
#' based on a custom function 
#' 
#' @param data data where it contains cached `filePath` of the table attached files
#' @param visualization_funs custom visualization function
#' @param output_location where to output the processed files location
#' 
#' @return a dataframe containing processed files
visualize_column_files <- function(data, 
                                   visualization_funs, 
                                   output_location){
  data %>% 
    dplyr::mutate(
    imagePath = purrr::map_chr(
      .x = filePath, 
      output_location = output_location,
      .f = visualization_funs))
}


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
#' @examples 
#' batch_process_table_column_files(...)
batch_process_table_column_files <- function(syn, all_data, curated_data, 
                                             synapse_tbl_id, filehandle_cols, 
                                             uid, survey_colnames,
                                             keep_metadata, n_batch, sort_keys,
                                             output_location, cache_location,
                                             visualization_funs){
  
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
    get_unannotated_files_in_batch(
      syn = syn,
      data = .,
      uid = uid,
      synapse_tbl_id = synapse_tbl_id,
      filehandle_cols = filehandle_cols,
      keep_metadata = keep_metadata,
      n_batch = n_batch,
      output_location = output_location,
      cache_location = cache_location) %>% 
    
    # visualize data
    visualize_column_files(
      data = .,
      visualization_funs = visualization_funs,
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