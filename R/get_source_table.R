#' Get source table
#' 
#' Get source table from synapse and pivot
#' table into tidy format based on filehandle
#' columns
#' 
#' @import tibble
#' @import glue
#' @import config
#' 
#' 
#' @param syn synapseclient
#' @param filehandle_cols column filehandles
#' @param synapse_tbl_id synapse ID of Table
#' 
#' @export
#' 
#' @return dataframe of tidy source synapse table
get_source_table <- function(syn, filehandle_cols, synapse_tbl_id){
    syn$tableQuery(glue::glue("SELECT * FROM {synapse_tbl_id}"))$asDataFrame() %>%
        tibble::as_tibble(.) %>%
        tidyr::pivot_longer(cols = all_of(filehandle_cols), 
                            names_to = "fileColumnName", 
                            values_to = "fileHandleId") %>%
        dplyr::filter(!is.na(fileHandleId)) %>%
        dplyr::mutate(
            createdOn = as.POSIXct(createdOn/1000, 
                                   origin="1970-01-01"),
            fileHandleId = as.character(fileHandleId))
}