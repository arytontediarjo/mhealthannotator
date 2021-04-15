visualize_psorcast_photo <- function(filePath){
    #' place your visualizer 
    new_dir <- glue::glue("images")
    if(!dir.exists(new_dir)){
        dir.create(new_dir) 
    }
    file.copy(filePath, new_dir)
    new_filePath <- file.path(new_dir, basename(filePath))
    if(tools::file_ext(new_filePath) == "png"){
        new_filePath <- change_png_to_jpeg(new_filePath)
    }
    return(new_filePath)
}

change_png_to_jpeg <- function(filePath){
    new_filePath <- sub('\\.png$', '.jpg', filePath)
    png_mat <- png::readPNG(filePath)
    jpeg::writeJPEG(png_mat, target = new_filePath, quality = 1)
    file.remove(filePath)
    return(new_filePath)
}
