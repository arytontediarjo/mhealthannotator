get_current_annotator <- function(syn){
  return(syn$getUserProfile()$userName)
}

get_output_filename <- function(annotator, filename){
  return(glue::glue("{annotator}_{filename}"))
}

#' @import tibble
#' @import magrittr
#' @import purrr
#' @import config
#' @import data.table
get_prev_curated_images <- function(syn, 
                                    parent_id,
                                    stored_filename,
                                    uid,
                                    keep_metadata,
                                    survey_colnames){
  
  schema <- tibble::tibble()
  schema <- purrr::map(c(uid, keep_metadata, survey_colnames), function(col){
    schema %>% 
      dplyr::mutate(!!sym(col) := as.character(NA))}) %>% 
    purrr::reduce(cbind) %>% 
    dplyr::mutate(fileColumnName = as.character(NA)) %>%
    tibble::as_tibble()
  image_data <- tryCatch({
    file_entity <- syn$getChildren(parent = parent_id) %>% 
      reticulate::iterate(., f = function(x){
        tibble::tibble(
          id = x$id, 
          fileName = x$name)}) %>% 
      purrr::reduce(., rbind) %>%
      dplyr::filter(fileName == stored_filename) %>%
      .$id %>% syn$get(.)
    data <- data.table::fread(file_entity$path, sep = "\t") %>% 
      tibble::as_tibble() %>%
      dplyr::mutate_all(as.character)
    return(data)
  }, error = function(e){
    return(schema)
  })
  return(image_data)
}

#' @import tibble
#' @import glue
#' @import config
get_all_image_source <- function(syn, filehandle_cols, synapse_tbl){
  all_image_data <- syn$tableQuery(glue::glue("SELECT * FROM {synapse_tbl}"))$asDataFrame() %>%
    tibble::as_tibble(.) %>%
    tidyr::pivot_longer(cols = all_of(filehandle_cols), 
                        names_to = "fileColumnName", 
                        values_to = "fileHandleId") %>%
    dplyr::filter(!is.na(fileHandleId)) %>%
    dplyr::mutate(
      createdOn = as.POSIXct(createdOn/1000, origin="1970-01-01"),
      fileHandleId = as.character(fileHandleId)
    )
  return(all_image_data)
}


load_data <- function(syn, 
                      synapse_config,
                      survey_config,
                      values){
  #' get all data and previous data
  values$allDf <- get_all_image_source(
    syn = syn, 
    filehandle_cols = synapse_config$filehandle_cols,
    synapse_tbl = synapse_config$synapse_tbl)
  
  #' get previous image that has been curated
  values$curatedDf <- get_prev_curated_images(
    syn = syn,
    parent_id = synapse_config$output_parent_id,
    stored_filename = values$fileName,
    uid = synapse_config$uid,
    keep_metadata = synapse_config$keep_metadata,
    survey_colnames = survey_config$survey_colnames
  )
  return(values)
}
  
  
  