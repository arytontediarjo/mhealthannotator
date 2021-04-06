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
    tibble::as_tibble()
  image_data <- tryCatch({
    file_entity <- syn$getChildren(parent = parent_id) %>% 
      reticulate::iterate(., f = function(x){
        tibble::tibble(id = x$id, fileName = x$name)}) %>% 
      purrr::reduce(., rbind) %>%
      dplyr::filter(fileName == stored_filename) %>%
      .$id %>% syn$get(.)
    data <- data.table::fread(file_entity$path, sep = "\t")
    if(nrow(data) == 0){
      return(schema)
    }
    return(data)
  }, error = function(e){
    return(schema)
  }) %>% 
    dplyr::mutate_all(as.character)
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