store_to_synapse <- function(new_data, 
                             prev_data, 
                             current_annotator, 
                             syn, synapseclient){
    input_file <- "testing.tsv"
    new_data %>% 
        dplyr::mutate(
            annotator = current_annotator,
            createdOn = as.character(createdOn),
            annotationTimestamp = as.character(annotationTimestamp)) %>%
        dplyr::full_join(prev_data) %>%
        tidyr::drop_na() %>%
        write.table(input_file, 
                    sep = "\t", 
                    row.names=F, 
                    quote=F)
    file <- synapseclient$File(input_file, parentId = "syn21627984")
    syn$store(file)
    unlink(input_file)
}