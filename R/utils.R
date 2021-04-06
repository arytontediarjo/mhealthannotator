#' Function to parse unique id for iteration
#' THIS IS MODULAR
parse_uid_to_string <- function(uid){
    uid %>% 
        purrr::map_chr(., function(x){glue::glue("'", x, "'")}) %>%
        paste0(., collapse = ",") %>%
        glue::glue("(", ., ")")
}