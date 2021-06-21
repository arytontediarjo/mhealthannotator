#' Function to store annotator's table to synapse
#'
#' @param syn synapse client object
#' @param synapseclient synapseclient library
#' @param parent_id parent id to store dataframe
#' @param new_data current annotations dataframe
#' @param stored_data previously stored annotationx dataframe
#' @param current_annotator current annotator as file prefix
#' @param output_filename name of the data output
#' @param ... additional info, will be used for provenance
#' @return 
store_tbl_to_synapse <- function(syn, 
                                 synapseclient,
                                 parent_id,
                                 new_data, 
                                 stored_data, 
                                 current_annotator,
                                 output_filename,
                                 ...){
    new_data %>% 
        dplyr::select(-any_of(c("filePath", "imagePath"))) %>%
        dplyr::mutate(annotator = current_annotator) %>%
        dplyr::mutate_all(.funs = as.character) %>%
        dplyr::full_join((stored_data %>%
                              dplyr::mutate_all(.funs = as.character))) %>%
        tidyr::drop_na() %>%
        write.table(output_filename, 
                    sep = "\t", 
                    row.names=F, 
                    quote=F)
    file <- synapseclient$File(
        output_filename, 
        parentId = parent_id)
    syn$store(file, activity = synapseclient$Activity(...))
    unlink(output_filename)
}