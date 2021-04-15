store_to_synapse <- function(syn, 
                             synapseclient,
                             synapse_parent_id,
                             new_data, 
                             stored_data, 
                             current_annotator,
                             output_filename){
    new_data %>% 
        dplyr::mutate(
            annotator = current_annotator,
            createdOn = as.character(createdOn),
            annotationTimestamp = as.character(annotationTimestamp)) %>%
        dplyr::full_join((stored_data %>%
                             dplyr::mutate_all(.funs = as.character))) %>%
        tidyr::drop_na() %>%
        dplyr::select(-imagePath) %>%
        write.table(output_filename, 
                    sep = "\t", 
                    row.names=F, 
                    quote=F)
    file <- synapseclient$File(
        output_filename, 
        parentId = synapse_parent_id)
    syn$store(file)
    unlink(output_filename)
}