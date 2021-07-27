#' Get Stored Annotation
#' 
#' Get stored annotation in synapse,
#' it will search based on synapse parentId, the filename, and
#' the annotator (filename prefix). If not exist,
#' build an empty dataframe
#' 
#' @import tibble
#' @import purrr
#' @importFrom data.table fread
#' @importFrom magrittr `%>%`
#' 
#' @param syn synapseclient
#' @param parent_id synapse parent Id
#' @param stored_filename filename of the annotation
#' @param uid unique identifier of the files needed to be annotate
#' @param keep_metadata what metadata to keep from the table
#' @param survey_colnames column for the survey
#' 
#' @export
#' 
#' @return a dataframe containing several columns
#' of the annotation information (survey input, metadata, annotation timestamp)
get_stored_annotation <- function(syn, parent_id,
                                  stored_filename,
                                  uid, keep_metadata,
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