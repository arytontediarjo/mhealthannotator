#' App UI
#' 
#' Create the UI component of mhealthannotator Shiny App
#' @import shiny
#' @import shinydashboard
#' @param request Shiny request
#' @export
#' @examples
#' \dontrun{
#' shinyApp(ui = app_ui, server = app_server)
#' }
app_ui <- function(request) {
  header <- dashboardHeader(
    title = "Sage Bionetworks - Digital Health Image Annotator",
    titleWidth = 550)
  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem(h4("About"), 
               tabName = "about", 
               icon = icon("question-circle")),
      menuItem(h4("Annotator-App"), 
               icon = icon("user-nurse"), 
               tabName = "annotator"))
  )
  body <- dashboardBody(
    # Add resoources in www
    golem_add_external_resources(),
    
    # create tab items for UI
    tabItems(
      tabItem(tabName = "about",
              h1("Welcome to the mHealth Annotator App!"),
              div("This app will help you parse through Synapse Table Filehandles and"),
              div("visualize each file handles for mage annotations"),
              h2("Tutorial:"),
              div("- Go to the 'Annotator-App' Tab"),
              div("- You can go through each image by using '<-/->' button"),
              div("- Score accordingly based on each prompts on the right-side"),
              div("- When you are finished, you can save the images by pressing 'Save Results' button"),
              div("- After saving, we will fetch you more data into the session"),
      ),
      
      # annotator UI
      tabItem(
        tabName = "annotator",
        fluidRow(
          column(width = 3, infoBoxOutput("userBox", width = "200px")),
          column(width = 4, infoBoxOutput("progressBox", width = "300px")),
          column(width = 4, infoBoxOutput("totalCurated", width = "300px"))
        ),
        fluidRow(
          br(),
          column(width = 7, 
                 align = "center",
                 div(mod_render_image_ui("render_image_ui"))),
          column(width = 4,
                 offset = 1,
                 mod_survey_input_user_ui("survey_input_ui"),
                 br(),
                 div(
                   style = "display:inline-block; float:left",
                   actionButton("goPrev", "", icon = icon("arrow-left"), 
                                width = "100px")),
                 div(
                   style = "display:inline-block; float:left",
                   actionButton("goNext", "", icon = icon("arrow-right"), 
                                width = "100px")),
                 br(),
                 br(),
                 br(),
                 div(style = "display:inline-block; float:left", 
                     actionButton("save", "Save My Results",
                                  style = "color: white; background-color: SteelBlue",
                                  icon = icon("cloud-upload"),
                                  width = "200px")))
        ),
        fluidRow(
          br(),
          br(),
          br(),
          conditionalPanel(
            'input.dataset === "metadata-table"',
          ),
          tabsetPanel(id = 'dataset',
                      tabPanel(
                        "metadata", 
                        DT::dataTableOutput("metadata_table")))
        )
      )
    ),
    
    # instantiate waiter loading screen
    waiter::use_waiter(),
    waiter::waiter_show_on_load(
      html = tagList(
        img(src = "www/loading.gif"),
        h4("Retrieving Synapse information...")
      ),
      color = "#424874"
    )
  )
  
  # consolidata header, sidebar and body
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
golem_add_external_resources <- function(){
  
  golem::add_resource_path(
    'www', app_sys('app/www')
  )
  
  tags$head(
    shinyWidgets::useSweetAlert(),
    shinyjs::useShinyjs(),
    golem::favicon(),
    golem::activate_js(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'mhealthannotator'
    ),
    tags$script(src = "www/readCookie.js")
  )
}

