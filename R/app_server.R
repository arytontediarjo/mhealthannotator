#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import waiter
#' @import reticulate
#' @noRd
app_server <- function( input, output, session ) {
  syn <- synapseclient$Synapse()
  
  config_path <- file.path(
    "conf", golem::get_golem_options("annotator_config"))
  config <- config::get(file = config_path)
  
  #' read synapse configuratiton 
  filehandle_cols <- config$synapse_opts$column_target %>% 
    unlist(.) %>% 
    setNames(NULL)
  synapse_tbl <- config$synapse_opts$synID$tbl
  n_batch <- config$synapse_opts$n_batch
  uid <- config$synapse_opts$unique_identifier
  keep_metadata <- config$synapse_opts$keep_metadata %>% unlist(.) %>% setNames(NULL)
  output_parent_id <- config$synapse_opts$synID$parent_output
  output_filename <- config$synapse_opts$output_filename
  
  #' read survey configuration
  survey_colnames <- config$survey_opts %>% 
    purrr::map(function(survey){survey$colname}) %>% 
    base::unlist() %>% 
    purrr::set_names(NULL)
  
  #' read survey configuration
  button_type <- config$survey_opts %>% 
    purrr::map(function(survey){survey$type}) %>% 
    base::unlist() %>% 
    purrr::set_names(NULL)
  
  #' define reactive values
  values <- reactiveValues(
    ii=1,  # image index
    userInput = list(), # what is the user input
    totalImages = NA, # number of images
    allDf = NA,
    useDf = NA, # what is the dataframe being used for annotating
    curatedDf = NA, # dataframe that has already been stored in synapse
    sessionNumImages= NA, #current session number of image
    currentAnnotator = NA, # who is the current annotator
    parentId = NA, # what is the parent id 
    fileName = NA, # what is the filename
    postConfirm = FALSE # extra handler if already been confirmed
  )
  session$sendCustomMessage(type="readCookie", message=list())
  observeEvent(input$cookie, {
    # If there's no session token, prompt user to log in
    if (input$cookie == "unauthorized") {
      waiter::waiter_update(
        html = tagList(
          img(src = "www/synapse_logo.png", height = "120px"),
          h3("Looks like you're not logged in!"),
          span("Please ", a("login", href = "https://www.synapse.org/#!LoginPlace:0", target = "_blank"),
               " to Synapse, then refresh this page.")
        )
      )
    } else {
      ### login and update session; otherwise, notify to login to Synapse first
      tryCatch({
        syn$login(sessionToken = input$cookie, rememberMe = FALSE)
        
        #' update data after updating session
        values$currentAnnotator <- get_current_annotator(syn)
        values$fileName <- get_output_filename(
          filename = output_filename,
          annotator = values$currentAnnotator)
        
        #' get all data and previous data
        values$allDf <- get_all_image_source(
          syn = syn, 
          filehandle_cols = filehandle_cols,
          synapse_tbl = synapse_tbl)
        values$total_images <- values$allDf %>%nrow()
        values$curatedDf <- get_prev_curated_images(
          syn = syn,
          parent_id = output_parent_id,
          stored_filename = values$fileName,
          uid = uid,
          keep_metadata = keep_metadata,
          survey_colnames = survey_colnames
        )
        
        #' batch process filehandles
        values$useDf <- batch_process_filehandles(syn = syn,
                                  values = values,
                                  synapse_tbl = synapse_tbl,
                                  filehandle_cols = filehandle_cols,
                                  uid = uid, 
                                  survey_colnames = survey_colnames,
                                  keep_metadata = keep_metadata,
                                  n_batch = n_batch)
        
        #' get number images
        values$numImages <- values$useDf %>% nrow(.)
        
        #' return feedback message if all images are annotated
        if(nrow(values$curatedDf) == nrow(values$allDf)){
          waiter_update(
            html = tagList(
              img(src = "www/synapse_logo.png", height = "120px"),
              h2("Apparently, we ran out of images to annotate:"),
              h3("Come back next time!")
            )
          )
          return("")
        }
        
        #' update waiter loading screen once login successful
        waiter::waiter_update(
          html = tagList(
            img(src = "www/loading.gif"),
            h4(sprintf("Retrieving Synapse information...")),
            h4(sprintf("Retrieving Images from Synapse..."))
          )
        )
        
        #' update waiter loading screen once login successful
        waiter::waiter_update(
          html = tagList(
            img(src = "www/synapse_logo.png", height = "120px"),
            h3(sprintf("Welcome, %s!", syn$getUserProfile()$userName))
          )
        )
        Sys.sleep(3)
        waiter::waiter_hide()
      }, error = function(err) {
        #' get error message
        Sys.sleep(2)
        error_msg <- stringr::str_squish(
          stringr::str_replace_all(geterrmessage(), "\n", ""))
        
        # get error logs and save to log/*
        tmp <- file.path(
          "log",
          glue::glue(
            gsub('[[:punct:] ]+',"", lubridate::now() %>% as.character(.)), 
            "_error.log"))
        sink(tmp)
        cat(glue::glue("ERROR: {error_msg}"))
        sink()
        
        #' update using waiter if there is error message in logfile
        waiter::waiter_update(
          html = tagList(
            img(src = "synapse_logo.png", height = "120px"),
            h2("ERROR:"),
            span("Please check your errors in error log directory (log/)")
          )
        )
      })
      
      # Any shiny app functionality that uses synapse should be within the
      # input$cookie observer
      output$title <- renderUI({
        titlePanel(sprintf("Welcome, %s", syn$getUserProfile()$userName))
      })
    }
  })
  
  #######################
  #' render user box
  #######################
  output$userBox <- renderInfoBox({
    infoBox(
      "Annotator", values$currentAnnotator, icon = icon("user"),
      color = "orange"
    )
  })

  #########################
  #' render progress box
  #########################
  output$progressBox <- renderInfoBox({
    total_curated <- (values$useDf %>% tidyr::drop_na() %>% nrow(.))
    infoBox(
      "Session Progress", glue::glue(total_curated,
                                     "/",values$numImages,
                                     " (", round(100 * total_curated/values$numImages, 1),"% Annotated)"),
      icon = icon("percentage"),
      color = "green"
    )
  })

  #########################
  #' render total curated
  #########################
  output$totalCurated <- renderInfoBox({
    perc_curated <- (values$curatedDf %>% nrow())/(values$total_images)
    infoBox(
      "Total Curation in Synapse", glue::glue(values$curatedDf %>% nrow(),
                                              "/", values$total_images,
                                              " (", round(100 * perc_curated, 1),"% Annotated)"),
      icon = icon("tasks"),
      color = "purple"
    )
  })

  ##############################################
  #' render survey prompt module
  ##############################################
  callModule(mod_survey_input_user_server, "ui_1", values = values)
  callModule(mod_render_image_server, "render_image_ui_1",
             obj_path = values$useDf$imagePath[values$ii])

  #################
  #' rended go forward button
  #################
  observeEvent(input$goNext, {
    values$useDf <- values$useDf %>%
      survey_input_store(curr_index = values$ii, 
                         user_inputs = values$userInput,
                         survey_colnames = survey_colnames,
                         keep_metadata = keep_metadata,
                         uid = uid)
    callModule(mod_render_image_server, "render_image_ui_1",
               obj_path = values$useDf$imagePath[values$ii])
    total_curated <- (values$useDf %>% tidyr::drop_na() %>% nrow(.))
    if((total_curated == values$numImages) & !values$postConfirm){
      ask_confirmation(
        inputId = "confirmation",
        title = "Thank You!! \n You have finished this session!",
        btn_labels = c("Review before saving", "Save to Synapse"),
        btn_colors = c("#FE642E", "#04B404"),
        type = "success")
      values$postConfirm <- TRUE
    }

    if(values$ii == values$useDf %>% nrow(.)){
      tmpI <- 1
    } else{
      tmpI <- values$ii + 1
    }
    values$ii <- tmpI
    values$userInput <- list()
    update_buttons(session = session, 
                   data = values$useDf, 
                   curr_index = values$ii,
                   survey_config = config$survey_opts)
  })

  #################
  #' render go back button
  ##################
  observeEvent(input$goPrev, {
    values$useDf <- values$useDf %>%
      survey_input_store(curr_index = values$ii, 
                         user_inputs = values$userInput,
                         survey_colnames = survey_colnames,
                         keep_metadata = keep_metadata,
                         uid = uid)
    callModule(
      mod_render_image_server, "render_image_ui_1",
      obj_path = values$useDf$imagePath[values$ii])
    total_curated <- (values$useDf %>% tidyr::drop_na() %>% nrow(.))
    if((total_curated == values$numImages) & !values$postConfirm){
      ask_confirmation(
        inputId = "confirmation",
        title = "Thank You!! \n You have finished your annotation!",
        btn_labels = c("Review before saving", "Save to Synapse"),
        btn_colors = c("#FE642E", "#04B404"),
        type = "success")
      values$postConfirm <- TRUE
    }
    if(values$ii > 1){
      tmpI <- values$ii - 1
    }else{
      tmpI <- values$useDf %>% nrow(.)
    }
    values$ii <- tmpI
    values$userInput <- list()
    update_buttons(session = session, 
                   data = values$useDf, 
                   curr_index = values$ii,
                   survey_config = config$survey_opts)
  })

  ##################################
  #' render save button
  ##################################
  observeEvent(input$save, {
    req(input$save)

    store_to_synapse(
      new_data = values$useDf,
      prev_data = values$curatedDf,
      current_annotator = values$currentAnnotator,
      syn = syn,
      synapseclient = synapseclient
    )

    Sys.sleep(3)
    resetLoadingButton("save")
    sendSweetAlert(
      session = session,
      title = "Thank you for collaborating with us!!",
      text = span(glue::glue("Data is saved in Synapse.")),
      type = "success"
    )
  })
  
  
  ##################################
  #' render data table
  ##################################
  output$mytable = DT::renderDataTable({
    data <- values$useDf[values$ii,] %>%
      dplyr::select(all_of(keep_metadata), all_of(survey_colnames))
    DT::datatable(data, options = list(searching = FALSE,
                                    lengthChange= FALSE))
  })
  
  observe({
    print(values$userInput)
  })
}
