#' user_alerts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_user_alerts_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' user_alerts Server Function
#'
#' @noRd 
mod_user_alerts_server <- function(input, output, session){
  ns <- session$ns
 
}
    
## To be copied in the UI
# mod_user_alerts_ui("user_alerts_ui_1")
    
## To be copied in the server
# callModule(mod_user_alerts_server, "user_alerts_ui_1")
 
