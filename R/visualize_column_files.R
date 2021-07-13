#' Visualize Synapse Table Column Files
#' 
#' Helper function to visualize synapse column files
#' based on a custom function 
#' 
#' @param data data where it contains cached `filePath` of the table attached files
#' @param funs custom visualization function
#' @param output_location where to output the processed files location
#' 
#' @return a dataframe containing processed files
visualize_column_files <- function(data, funs, output_location){
    funs <- eval(parse(text = funs))
    data %>%  
        dplyr::mutate(
            basePath = purrr::map_chr(
                filePath, function(fp){
                    file.copy(fp, output_location)
                    return(basename(fp))}),
            imagePath = file.path(output_location, basePath),
            imagePath = purrr::map_chr(
                imagePath, .f = funs))
}
