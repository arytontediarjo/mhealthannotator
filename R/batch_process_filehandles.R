#' @import stats
#' @import tidyr
#' @import ggplot2
#' @import magrittr
get_image_batch <- function(syn, 
                            data,
                            synapse_tbl,
                            filehandle_cols,
                            uid, 
                            keep_metadata,
                            n_batch,
                            parallel = FALSE,
                            output_location,
                            cache_directory){
  syn$cache$cache_root_dir <- "user_dir/atediarjo"
  get_subset <- data %>%
    dplyr::slice(1:n_batch) %>%
    .[[uid]] %>% 
    parse_uid_to_string()
  entity <- syn$tableQuery(
    glue::glue(
      "SELECT * FROM {synapse_tbl} WHERE recordId IN {get_subset}"))
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
    dplyr::select(all_of(keep_metadata), 
                  all_of(uid), 
                  fileColumnName, 
                  filePath) %>%
    dplyr::mutate(
      imagePath = purrr::map_chr(
        .x = filePath, 
        output_location = output_location,
        .f = golem::get_golem_options("visual_funs"))) %>%
    dplyr::mutate_all(as.character) %>%
    tidyr::drop_na(any_of(c("imagePath")))
  return(result)
}


#' batch_process_filehandle Server Function
#'
#' @noRd 
batch_process_filehandles <- function(syn,
                                      values,
                                      synapse_tbl,
                                      filehandle_cols,
                                      uid, 
                                      survey_colnames,
                                      keep_metadata,
                                      n_batch){
  #' retrieve images
  result <- values$allDf %>%
    dplyr::anti_join(
      values$curatedDf,
      by = c(uid, "fileColumnName"))  %>%
    get_image_batch(syn = syn,
                    data = .,
                    uid = uid,
                    synapse_tbl = synapse_tbl,
                    filehandle_cols = filehandle_cols,
                    keep_metadata = keep_metadata,
                    n_batch = n_batch,
                    parallel = FALSE,
                    output_location = file.path("user_dir",
                                                values$currentAnnotator,
                                                "processed_files"),
                    cache_directory = file.path("user_dir",
                                                values$currentAnnotator,
                                                "downloaded_files")) %>% 
    dplyr::bind_cols(
      (survey_tbl <- purrr::map_dfc(
        survey_colnames, function(x){
        tibble(!!sym(x) := as.character(NA))
      }))) %>%
    dplyr::mutate(annotationTimestamp = NA_character_)
  return(result)
}
    
## To be copied in the UI
# mod_batch_process_filehandle_ui("batch_process_filehandle_ui_1")
    
## To be copied in the server
# callModule(mod_batch_process_filehandle_server, "batch_process_filehandle_ui_1")
 
