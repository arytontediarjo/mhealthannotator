#' render_image UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_render_image_ui <- function(id){
  ns <- NS(id)
  tagList(
    imageOutput(ns("image"))
  )
}
    
#' render_image Server Function
#'
#' @noRd 
mod_render_image_server <- function(input, output, session, obj_path){
  ns <- session$ns
  
  output$image <- renderImage({
    # set dynamic sizing
    width  <- session$clientData$output_image_width 
    height <- session$clientData$output_image_height
    pixelratio <- 2
    
    # A temp file to save the output.
    outfile <- tempfile(fileext='.jpg')
    
    # Generate the image file
    jpeg(outfile, res = 72*pixelratio)
    dev.off()
    list(src = obj_path,
         height = "450px",
         width  = "650px")
  }, deleteFile=FALSE)
 
}
    
## To be copied in the UI
# mod_render_image_ui("render_image_ui_1")
    
## To be copied in the server
# callModule(mod_render_image_server, "render_image_ui_1")
 
