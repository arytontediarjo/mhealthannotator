store_to_synapse <- function(syn, 
                             synapseclient,
                             synapse_parent_id,
                             new_data, 
                             stored_data, 
                             current_annotator,
                             output_filename, ...){
    args <- list(...)
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
        parentId = synapse_parent_id)
    syn$store(
        file, activityName = args$activityName, used = args$used)
    unlink(output_filename)
}