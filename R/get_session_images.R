#' @title Get Table Unique Identifier String Filter
#' 
#' @description This is a helper function to build string filter (SQL-like)
#' for filtering Synapse Table before downloading files 
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


#' @title Get unannotated files in batch
#' 
#' @description Helper function for downloading a number of Synapse table-attached 
#' files according to input batch, and process using desired function
#' 
#' @param data containing un-annotated data based on each user
#' @return unannotated data based on previously stored records
get_session_images <- function(data, syn, synapse_tbl_id, 
                               filehandle_cols, uid, 
                               keep_metadata, n_batch,
                               cache_location){
    
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
    syn$downloadTableColumns(
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
