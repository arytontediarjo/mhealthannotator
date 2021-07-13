#' Function to create user directory for temporarily
#' storing image files (to not overpopulate Shiny Server)
#' @param curr_annotator user name of the annotator
create_user_directory <- function(curr_annotator){
    #' create user directory
    user_dir <- file.path(
        "dir", curr_annotator)
    dir.create(
        user_dir, showWarnings = FALSE) 
    dir.create(
        file.path(user_dir, "downloaded_files"), 
        showWarnings = FALSE)
    dir.create(
        file.path(user_dir, "processed_files"), 
        showWarnings = FALSE)
}

#' Function to clear user directory
#' storing image files (to not overpopulate Shiny Server)
#' @param curr_annotator user name of the annotator
clear_user_directory <- function(curr_annotator){
    unlink(glue::glue(
        "dir/{curr_annotator}"), 
        recursive = T, force = T)
    unlink(glue::glue(
        "dir/{curr_annotator}"), 
        recursive = T, force = T)
}

