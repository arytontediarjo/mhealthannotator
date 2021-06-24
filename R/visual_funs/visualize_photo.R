visualize_photo <- function(filepath){
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