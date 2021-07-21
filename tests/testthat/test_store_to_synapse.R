#' test case for get source table
syn_objs <- attempt_instantiate()
syn <- syn_objs$syn
synapseclient <- syn_objs$synapseclient
tryCatch({
    attempt_login(syn)
    }, error = function(e) {
        print(glue::glue("Did not log into Synapse: {e$message}"))
    }
)
Sys.setenv(R_CONFIG_ACTIVE = "testing")

#' check if get source table results to a dataframe
test_that("get_source_table returns a data frame", {
    skip_if_not(logged_in(syn = syn))
    
    # some variables
    current_annotator <- "atediarjo"
    stored_data <- tibble::tibble(tmp = c(seq(1,5))) %>%
        dplyr::mutate(annotator = current_annotator)
    new_data <- tibble::tibble(tmp = c(seq(6,10)))
    parent_id <- "syn25946302"
    output_filename <- "test.tsv"
    
    # test function
    store_to_synapse(
        syn, 
        synapseclient, 
        parent_id = parent_id,
        new_data = new_data,
        stored_data = stored_data,
        current_annotator = current_annotator,
        output_filename = output_filename)
    
    # get stored data
    stored_syn_id <- syn$findEntityId(
        "test.tsv", parent = "syn25946302")
    
    # check if data is stored
    expect_true(inherits(stored_syn_id, "character"))
    
    # open data
    data <- fread(syn$get(stored_syn_id)$path) %>%
        dplyr::arrange(tmp)
    
    #' check if dataframe
    expect_true(inherits(data, "data.frame"))
    
    #' check if dataframe is appended correctly
    expect_equal(data$tmp, seq(1,10))
    
    #' check if data have the right annotator
    expect_equal(data$annotator, rep("atediarjo", 10))
})
