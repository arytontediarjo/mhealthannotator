#' function to create dataframe for sensor samples
create_data <- function(){
    tibble::tibble(t = seq(0,30, by = 0.01)) %>%
        dplyr::mutate(x = rnorm(nrow(.)),
                      y = rnorm(nrow(.)),
                      z = rnorm(nrow(.), mean = 1))
}

create_samples <- function(n){
    purrr::map_dfr(c(1:n), function(x){
        output <- glue::glue("sample_{x}.tsv", x = x)
        data <- create_data() %>% 
            readr::write_tsv(output)
        return(tibble(filePath = output))
    })
}

test_funs <- function(filePath){
    output_filepath <- file.path(
        glue::glue(gsub(
            "\\.tsv$", "", 
            filePath), ".jpg"))
    plot <- fread(filePath) %>%
        ggplot(aes(x = t, y = x)) + 
        geom_line()
    ggsave(filename = output_filepath, plot = plot)
    unlink(filePath)
    return(output_filepath)
}

clean_test_dir <- function(){
    purrr::walk(list.files("."), function(file){
        if(!stringr::str_detect(file, "^test.*.R$")){
            unlink(file)
        }
    })
    unlink("output", recursive = TRUE)
                
}

#' check if get source table results to a dataframe
test_that("visualize_column_files returns desired filepath output", {
    output_dir <- "output"
    dir.create(output_dir, showWarnings = FALSE)
    test_data <- create_samples(5) %>%
        visualize_column_files(test_funs, output_dir)
    output_target <- purrr::map_chr(test_data$imagePath, ~basename(.x))
    output_files <- list.files(output_dir)
    expect_equal(output_target, output_files)
    clean_test_dir()
})

