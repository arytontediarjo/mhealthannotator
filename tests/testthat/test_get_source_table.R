syn <- attempt_instantiate()
tryCatch(
    attempt_login(syn),
    error = function(e) {
        print(glue::glue("Did not log into Synapse: {e$message}"))
    }
)

#' check if get source table results to a dataframe
test_that("get_source_table returns a data frame", {
    skip_if_not(logged_in(syn = syn))
    result <- get_source_table(syn = syn, 
                               filehandle_cols = c(
                                   "psoriasisAreaPhoto.jpg",
                                   "psoriasisAreaPhoto.png"),
                               synapse_tbl_id = "syn22281748")
    expect_true(inherits(result, "data.frame"))
})
