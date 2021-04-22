parse_picture_to_jpeg <- function(filepath, output_location){
    file.copy(filepath, output_location)
    filepath <- file.path(output_location, basename(filepath))
    if(tools::file_ext(filepath) == "png"){
        new_filepath <- sub('\\.png$', '.jpg', filepath)
        png_mat <- png::readPNG(filepath)
        jpeg::writeJPEG(png_mat, target = new_filepath, quality = 1)
        file.remove(filepath)
        return(new_filepath)
    }else{
        return(filepath)
    }
}

visualize_photo <- function(filepath, output_location){
    parse_picture_to_jpeg(filepath, output_location)
}
