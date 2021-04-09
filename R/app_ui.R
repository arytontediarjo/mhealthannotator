#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @import shinyFeedback
#' @noRd
app_ui <- function(request) {
  header <- dashboardHeader(
    title = "Sage Bionetworks - Digital Health Image Annotator",
    titleWidth = 550)
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(h4("About"), tabName = "about", icon = icon("question-circle")),
      menuItem(h4("Annotator-App"), icon = icon("user-nurse"), tabName = "annotator"))
  )
  body <- dashboardBody(
    golem_add_external_resources(),
    tabItems(
      tabItem(tabName = "about",
              h1("Welcome to the mHealth Annotator App!"),
              div("This app will help you parse through Synapse Filehandles and"),
              div("visualize each file handle for annotating image"),
              h2("Tutorial:"),
              div("- You can go through each image by using '<-/->' button"),
              div("- When you are done you can save the images by pressing 'Save Results' button"),
              div("- When you ran out of images you can press 'Refresh Session' to get more images"),
              div("- We will not log your image annotations if you have `None Selected` for both prompts")
      ),
      tabItem(
        tabName = "annotator",
        fluidRow(
          column(width = 3, infoBoxOutput("userBox", width = "300px")),
          column(width = 3, infoBoxOutput("progressBox", width = "300px")),
          column(width = 3, infoBoxOutput("totalCurated", width = "300px"))),
        br(),
        fluidRow(
          column(width = 6, mod_render_image_ui("render_image_ui_1")),
          column(width = 5, offset = 1, 
                 box(mod_survey_input_user_ui("ui_1"),
                     br(),
                     br(),
                     div(style = "display:inline-block; float:left",
                         actionButton("goPrev", "", 
                                      icon = icon("arrow-left"),
                                      width = "100px")),
                     div(style = "display:inline-block; float:left",
                         actionButton("goNext", "", 
                                      icon = icon("arrow-right"),
                                      width = "100px")),
                     width = 100, height = 400),
                 br(),
                 div(style = "display:inline-block; float:right", 
                     loadingButton(
                       "refresh", "Refresh Session")),
                 div(style = "display:inline-block; float:left", 
                     loadingButton(
                       "save", "Save My Results"))
                 )
        ),
        fluidRow(
          conditionalPanel(
            'input.dataset === "image-metadata"',
          ),
          conditionalPanel(
            'input.dataset === "additional-info"',
          ),

          tabsetPanel(id = 'dataset',
                      tabPanel("image-metadata", DT::dataTableOutput("mytable")),
                      tabPanel("additional-info", DT::dataTableOutput("featuretable"))),
        )
      )
    ),
    ## waiter loading screen
    waiter::use_waiter(),
    waiter::waiter_show_on_load(
      html = tagList(
        img(src = "www/loading.gif"),
        h4("Retrieving Synapse information...")
      ),
      color = "#424874"
    )
  )
  dashboardPage(
    header,
    sidebar,
    body
  )
  
    
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  golem::add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs(),
    shinybusy::add_busy_spinner(
      spin = "circle",
      color = "#112446",
      timeout = 100,
      position = c("bottom-right"),
      onstart = TRUE,
      margins = c(10, 10),
      height = "50px",
      width = "50px"
    ),
    golem::favicon(),
    golem::activate_js(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'shinygolemtest'
    ),
    tags$script(src = "www/readCookie.js")
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

