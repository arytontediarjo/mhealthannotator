parse_picture_to_jpeg <- function(filepath, user_dir){
    file.copy(filepath, user_dir)
    filepath <- file.path(user_dir, basename(filepath))
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

visualize_photo <- function(filepath, curr_annotator){
    #' create individual user directory
    img_dir <- "user_images"
    user_dir <- create_user_directory(img_dir, curr_annotator)
    
    #' parse your function here
    filepath <- parse_picture_to_jpeg(filepath, user_dir)
    
    #' return image
    return(filepath)
}
