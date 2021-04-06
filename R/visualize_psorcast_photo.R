visualize_psorcast_photo <- function(filePath){
    #' place your visualizer 
    new_dir <- glue::glue("images")
    if(!dir.exists(new_dir)){
        dir.create(new_dir) 
    }
    file.copy(filePath, new_dir)
    new_filePath <- file.path(new_dir, basename(filePath))
    return(new_filePath)
}
