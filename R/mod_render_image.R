#' @title UI Function for the image rendering
#'
#' @description Creates the UI for image rendering
#' 
#' @export
#' 
#' @rdname render_image
#' 
#' @param id the id
#'
#' @return UI for the image rendering for the dashboard
mod_render_image_ui <- function(id){
  ns <- NS(id)
  tagList(
    imageOutput(ns("image"))
  )
}
    
#' @title Server Function for the image rendering
#'
#' @description Creates the server for image rendering, complete with 
#' customizable image width/height
#' 
#' @export
#' 
#' @rdname render_image
#' 
#' @param input the input variables from [shiny::callModule()]
#' @param output the output variables from [shiny::callModule()]
#' @param session the session from [shiny::callModule()]
#' @param obj_path image filepath
#' @param input_width image width
#' @param input_height image height
#'
#' @return
mod_render_image_server <- function(input, output, 
                                    session, obj_path,
                                    input_width = NULL,
                                    input_height = NULL){
  
  if(is.null(input_width)){
    input_width = "auto"
  }
  
  if(is.null(input_height)){
    input_height = "100%"
  }
  
  ns <- session$ns
  output$image <- renderImage({
    # set dynamic sizing
    pixelratio <- 2
    # A temp file to save the output.
    outfile <- tempfile(fileext='.jpg')
    # Generate the image file
    jpeg(outfile, res = 150*pixelratio)
    dev.off()
    list(src = obj_path,
         height = input_height,
         width  = input_width)
  }, deleteFile=FALSE)
}
