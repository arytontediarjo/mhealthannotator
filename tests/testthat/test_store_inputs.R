# test data
test_data <- tibble::tibble(
    uid = c("row_1", "row_2"),
    var_1 = c("test_1", "test_2"),
    var_2 = c("1", "2"),
    fileColumnName = c("fh_1", "fh_2"),
    imagePath = "path/to/file",
    annotationTimesamp = as.character(NA))

# test case for the dataframe
data <- tibble::tibble(
    uid = c("row_1", "row_2"),
    fileColumnName = c("fh_1", "fh_2"),
    var_1 = NA_character_,
    var_2 = NA_character_,
    annotationTimestamp = NA_character_,
    imagePath = "path/to/file")

#' test on storing input to first row
test_that("test_store_inputs on first row", {
  data <- data %>%
      store_inputs(curr_index = 1,
                   user_inputs = list(
                       var_1 = "test_1", 
                       var_2 = "1"),
                   keep_metadata =  c(), 
                   uid = "uid")
    
    # test first index store input
    expect_equal(data$var_1[[1]], test_data$var_1[[1]])
    expect_equal(data$var_2[[1]], test_data$var_2[[1]])
})

# report_results() -------------------------------------------------------------

#' test on storing input to second row
test_that("test_store_inputs on second index", {
    data <- data %>%
        store_inputs(curr_index = 2,
                     user_inputs = list(
                         var_1 = "test_2", 
                         var_2 = "2"),
                     keep_metadata =  c(), 
                     uid = "uid")
    
    # test second index store input
    expect_equal(data$var_1[[2]], test_data$var_1[[2]])
    expect_equal(data$var_2[[2]], test_data$var_2[[2]])
})

# show_details() ---------------------------------------------------------------