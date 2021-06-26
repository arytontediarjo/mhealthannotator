#' App Server
#' 
#' Create the server-side component of mhealthannotator app
#' 
#' @import shiny
#' @import shinydashboard
#' @import waiter
#' @import reticulate 
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @return none
#' @export
app_server <- function( input, output, session ) {
  
  # instantiate synapse
  syn <- synapseclient$Synapse()
  
  # read configuraiton file
  config_path <- file.path(golem::get_golem_options("config"))
  config <- config::get(file = config_path)
  
  # parse all configuration type
  synapse_config <- config %>% parse_synapse_opts()
  survey_config <- config %>% parse_survey_opts()
  image_config <- config %>% parse_image_opts()
  visualization_funs <-  golem::get_golem_options("funs")
  
  
  # define reactive values
  values <- reactiveValues(
    ii=1,  # image index
    userInput = list(), # what is the user input
    allDf = NA, # whole dataframe
    useDf = NA, # dataframe being used for current annotating session
    curatedDf = NA, # dataframe that has already been stored in synapse
    currentAnnotator = NA, # who is the current annotator
    parentId = NA, # what is the parent id 
    fileName = NA, # what is the filename
    postConfirm = FALSE, # extra handler if already been confirmed
    cacheLocation = NA,
    outputLocation = NA
  )
  
  # read synapse session cookie
  session$sendCustomMessage(type="readCookie", message=list())
  
  
  #########################
  # instantiate shiny app
  #########################
  observeEvent(input$cookie, {
    # If there's no session token, prompt user to log in
    if (input$cookie == "unauthorized") {
      waiter::waiter_update(
        html = tagList(
          img(src = "www/synapse_logo.png", height = "120px"),
          h3("Looks like you're not logged in!"),
          span("Please ", 
               a("login", href = "https://www.synapse.org/#!LoginPlace:0", 
                 target = "_blank"),
               " to Synapse, then refresh this page.")
        )
      )
    } else {
      ### login and update session; otherwise, notify to login to Synapse first
      tryCatch({
        syn$login(sessionToken = input$cookie, rememberMe = FALSE)
        
        # update data after updating session
        values$currentAnnotator <- syn$getUserProfile()$userName
        values$fileName <-glue::glue(
          "{annotator}_{filename}",filename = synapse_config$output_filename,
          annotator = values$currentAnnotator)
        
        # create log directory
        dir.create("log", showWarnings = FALSE)
        dir.create("dir", showWarnings = FALSE) 
        
        # create user directory
        clear_directory(values$currentAnnotator)
        create_user_directory(values$currentAnnotator)
        
        # set cache and output location based on user
        values$cacheLocation <- file.path(
          "dir", values$currentAnnotator, "downloaded_files")
        values$outputLocation <- file.path(
          "dir", values$currentAnnotator, "processed_files")
        
        #' get all data and previous data
        values$allDf <- get_source_table(
          syn = syn, 
          filehandle_cols = synapse_config$filehandle_cols,
          synapse_tbl_id = synapse_config$synapse_tbl_id)
        
        #' get previous image that has been curated
        values$curatedDf <- get_stored_annotation(
          syn = syn,
          parent_id = synapse_config$output_parent_id,
          stored_filename = values$fileName,
          uid = synapse_config$uid,
          keep_metadata = synapse_config$keep_metadata,
          survey_colnames = survey_config$survey_colnames
        )
        
        # check if user has annotated everything
        if(nrow(values$curatedDf) ==  nrow(values$allDf)){
          waiter_update(
            html = tagList(
              img(src = "www/synapse_logo.png", height = "120px"),
              h2("You have gotten through all the images, great work!"),
              h3("Come again next time!")
            )
          )
          return("")
        }
        
        # batch process filehandles
        values$useDf <- batch_process_table_column_files(
          syn = syn,
          all_data = values$allDf,
          curated_data = values$curatedDf,
          synapse_tbl_id = synapse_config$synapse_tbl_id,
          filehandle_cols = synapse_config$filehandle_cols,
          uid = synapse_config$uid, 
          survey_colnames = survey_config$survey_colnames,
          keep_metadata = synapse_config$keep_metadata,
          n_batch = synapse_config$n_batch,
          sort_keys = synapse_config$sort_keys,
          output_location = values$outputLocation,
          cache_location = values$cacheLocation,
          visualization_funs = visualization_funs
        )
        
        # return feedback message if all images are annotated
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
        
        # update waiter loading screen once login successful
        waiter::waiter_update(
          html = tagList(
            img(src = "www/loading.gif"),
            h4(sprintf("Retrieving Synapse information...")),
            h4(sprintf("Retrieving Images from Synapse..."))
          )
        )
        
        # update waiter loading screen once login successful
        waiter::waiter_update(
          html = tagList(
            img(src = "www/synapse_logo.png", height = "120px"),
            h3(sprintf("Welcome, %s!", syn$getUserProfile()$userName))
          )
        )
        Sys.sleep(3)
        waiter::waiter_hide()
      }, error = function(err) {
        # get error message
        Sys.sleep(2)
        error_msg <- stringr::str_squish(geterrmessage())
        
        # get error logs and save to logs
        tmp <- file.path(
          "log",
          glue::glue(
            gsub('[[:punct:] ]+',"", 
                 lubridate::now() %>% 
                   as.character(.)), 
            "_error.log"))
        sink(tmp)
        cat(glue::glue("ERROR: {error_msg}"))
        sink()
        
        # update using waiter if there is error message in logfile
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
  # render user box
  #######################
  output$userBox <- renderInfoBox({
    infoBox(
      "Annotator", values$currentAnnotator, icon = icon("user"),
      color = "orange"
    )
  })

  #########################
  # render progress box
  #########################
  output$progressBox <- renderInfoBox({
    total_curated <- (values$useDf %>% tidyr::drop_na() %>% nrow(.))
    infoBox(
      "Session Progress", glue::glue(total_curated,
                                     "/", nrow(values$useDf),
                                     " (", round(100 * total_curated/nrow(values$useDf), 1),"% Annotated)"),
      icon = icon("percentage"),
      color = "green"
    )
  })

  #########################
  # render total curated
  #########################
  output$totalCurated <- renderInfoBox({
    perc_curated <- (values$curatedDf %>% nrow())/(nrow(values$allDf))
    infoBox(
      "Total Curation in Synapse", glue::glue(values$curatedDf %>% nrow(),
                                              "/", nrow(values$allDf),
                                              " (", round(100 * perc_curated, 1),"% Annotated)"),
      icon = icon("tasks"),
      color = "purple"
    )
  })

  ##############################################
  # render survey prompt module
  ##############################################
  callModule(mod_survey_input_user_server, 
             "survey_input_ui", 
             survey_colnames = survey_config$survey_colnames,
             values = values)
  callModule(mod_render_image_server, 
             "render_image_ui",
             obj_path = values$useDf$imagePath[values$ii],
             input_width = image_config$width,
             input_height = image_config$height)

  ##################################
  # render go forward button
  ##################################
  observeEvent(input$goNext, {
    if("" %in% values$userInput){
      sendSweetAlert(
        session,
        title = "Oops!",
        text = "Please finish all the survey questions before moving to the next one",
        type = "error",
        btn_labels = "Ok",
        btn_colors = "#3085d6",
        html = FALSE,
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE,
        width = NULL
      )
    }else{
      # store survey input 
      values$useDf <- values$useDf %>%
        survey_input_store(
          curr_index = values$ii, 
          user_inputs = values$userInput,
          survey_colnames = survey_config$survey_colnames,
          keep_metadata = synapse_config$keep_metadata,
          uid = synapse_config$uid
        )
      
      # call module to render image
      callModule(mod_render_image_server, 
                 "render_image_ui",
                 obj_path = values$useDf$imagePath[values$ii],
                 input_width = image_config$width,
                 input_height = image_config$height)
      
      total_curated <- (values$useDf %>% tidyr::drop_na() %>% nrow(.))
      if((total_curated == nrow(values$useDf)) & !values$postConfirm){
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
      values <- update_buttons(
        reactive_values = values,
        session = session, 
        curr_index = values$ii,
        config = config$survey_opts)
      
    }
  })

  #################
  # render go back button
  ##################
  observeEvent(input$goPrev, {
    if("" %in% values$userInput){
      sendSweetAlert(
        session,
        title = "Oops!",
        text = "Please finish all the survey questions before moving to the next one",
        type = "error",
        btn_labels = "Ok",
        btn_colors = "#3085d6",
        html = FALSE,
        closeOnClickOutside = TRUE,
        showCloseButton = FALSE,
        width = NULL
      )
    }else{
      values$useDf <- values$useDf %>%
        survey_input_store(curr_index = values$ii, 
                           user_inputs = values$userInput,
                           survey_colnames = survey_config$survey_colnames,
                           keep_metadata = synapse_config$keep_metadata,
                           uid = synapse_config$uid)
      callModule(mod_render_image_server, 
                 "render_image_ui",
                 obj_path = values$useDf$imagePath[values$ii],
                 input_width = image_config$width,
                 input_height = image_config$height)
      total_curated <- (values$useDf %>% tidyr::drop_na() %>% nrow(.))
      if((total_curated == nrow(values$useDf)) & !values$postConfirm){
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
      values <- update_buttons(
        reactive_values = values,
        session = session, 
        curr_index = values$ii,
        config = config$survey_opts)
    }
  })
  
  ##################################
  # ask for confirmation
  ##################################
  observeEvent(input$confirmation, {
    if(input$confirmation){
      shinyjs::click(id = "save")
    }
  })

  ##################################
  # render save button
  ##################################
  observeEvent(input$save, {
    req(input$save)
    
    # reset post confirmation
    values$postConfirm <- FALSE
    
    # clear directory
    clear_directory("user_dir", values$currentAnnotator)
    
    # show modal spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      text = shiny::tagList(
        h3("Please Wait..."),
        h4("We are uploading your data to Synapse."))
    )
    
    # save to synapse
    store_tbl_to_synapse(
      syn = syn,
      synapseclient = synapseclient,
      parent_id = synapse_config$output_parent_id,
      new_data = values$useDf,
      stored_data = values$curatedDf,
      current_annotator = values$currentAnnotator,
      output_filename = values$fileName
    )
    
    # remove when done
    Sys.sleep(2)
    shinybusy::remove_modal_spinner()
    
    # show modal spinner
    shinybusy::show_modal_spinner(
      spin = "fading-circle",
      text = shiny::tagList(
        h3("Please Wait..."),
        h4("We are fetching more data..."))
    )
    
    # reset to 1
    values$ii <- 1
    
    #' get all data and previous data
    values$allDf <- get_source_table(
      syn = syn, 
      filehandle_cols = synapse_config$filehandle_cols,
      synapse_tbl_id = synapse_config$synapse_tbl_id)
    
    #' get previous image that has been curated
    values$curatedDf <- get_stored_annotation(
      syn = syn,
      parent_id = synapse_config$output_parent_id,
      stored_filename = values$fileName,
      uid = synapse_config$uid,
      keep_metadata = synapse_config$keep_metadata,
      survey_colnames = survey_config$survey_colnames
    )
    
    # refresh if ran out of images
    if(nrow(values$allDf) == nrow(values$curatedDf)){
      shinyjs::refresh()
    }else{
      # batch process filehandles
      values$useDf <- batch_process_table_column_files(
        syn = syn,
        all_data = values$allDf,
        curated_data = values$curatedDf,
        synapse_tbl_id = synapse_config$synapse_tbl_id,
        filehandle_cols = synapse_config$filehandle_cols,
        uid = synapse_config$uid, 
        survey_colnames = survey_config$survey_colnames,
        keep_metadata = synapse_config$keep_metadata,
        n_batch = synapse_config$n_batch,
        sort_keys = synapse_config$sort_keys,
        output_location = values$outputLocation,
        cache_location = values$cacheLocation,
        visualization_funs = visualization_funs
      )
      
      # update buttons
      values <- update_buttons(
        reactive_values = values,
        session = session, 
        curr_index = values$ii,
        config = config$survey_opts)
      
      # re-render image
      callModule(mod_render_image_server, 
                 "render_image_ui",
                 obj_path = values$useDf$imagePath[values$ii],
                 input_width = image_config$width,
                 input_height = image_config$height)
      
      # remove when done
      Sys.sleep(2)
      shinybusy::remove_modal_spinner()
      
      # send sweet alert
      sendSweetAlert(
        session = session,
        title = "Session is updated!",
        text = "We saved your previous session annotations to Synapse.",
        type = "success"
      )
    }
  })
  
  ##################################
  # render data table
  ##################################
  output$metadata_table = DT::renderDataTable({
    data <- values$useDf[values$ii,] %>%
      dplyr::select(
        all_of(synapse_config$uid),
        all_of(synapse_config$keep_metadata), 
        all_of(survey_config$survey_colnames),
        fileColumnName,
        annotationTimestamp)
    DT::datatable(
      data, options = list(
        searching = FALSE, 
        scrollX = TRUE,
        lengthChange= FALSE))
  })
}
