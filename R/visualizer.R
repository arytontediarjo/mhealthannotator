#' @title mpower gait visualization entry-point
#' 
#' @description this function is used to visualize a 
#' filepath for gait data in Synapse table
#' 
#' @import ggplot2
#' @import patchwork
#' @importFrom magrittr "%>%"
#' 
#' @param filepath the corresponding filepath being used
#' 
#' @return the output filepath for the visualization
visualize_mpower_gait <- function(filepath){
    shape_sensor_data <- function(filepath){
        ts <- filepath %>%
            jsonlite::fromJSON(.) %>%
            dplyr::filter(stringr::str_detect(
                tolower(sensorType), 
                "^accel|^rotationrate|^gravity|^gyro"))
        sensors_detected <- tolower(unique(ts$sensorType))
        if((stringr::str_detect(sensors_detected, "^gyro|^rotationrate") %>% sum(.)) == 2){
            ts  <- ts %>% 
                dplyr::filter(!stringr::str_detect(tolower(sensorType), "^gyro"))
        }
        ts <- ts %>%
            base::split(.$sensorType) %>%
            purrr::map(., function(ts){
                ts %>% 
                    dplyr::mutate(t = timestamp - .$timestamp[1]) %>%
                    dplyr::select(t,x,y,z)})
        return(ts)
    }
    
    get_color_mapping <- function(cols_key){
        accel_col <- cols_key[stringr::str_detect(cols_key, "^accel")]
        rotation_col <- cols_key[stringr::str_detect(cols_key, "^gyro|^rotation")]
        gravity_col <- cols_key[stringr::str_detect(cols_key, "^gravity")]
        mapping_list <- stats::setNames(c(accel_col, rotation_col, gravity_col),
                                        c(accel_col, rotation_col, gravity_col))
        
        color_mapping <- purrr::map(mapping_list, function(detected_sensor){
            if(stringr::str_detect(tolower(detected_sensor), "^accel")){
                "lightskyblue3"
            }else if(stringr::str_detect(tolower(detected_sensor), "^gyro|^rotation")){
                "orange"
            }else{
                "palegreen3"
            }
        })
        return(color_mapping)
    }
    output_filename <- tryCatch({
        output_filename <- file.path(
            glue::glue(gsub(
                "\\.json$", "", 
                filepath), ".jpg"))
        data <-  shape_sensor_data(filepath)
        sensor_col_map <-  get_color_mapping(names(data))
        plot <- purrr::map(names(data), function(sensor){
            color <- sensor_col_map[[sensor]]
            x <- data[[sensor]] %>% 
                ggplot(aes_string(x = "t", y = "x")) + 
                geom_line(color = color) + 
                geom_hline(yintercept = 0, linetype = "twodash") +
                theme_minimal() +
                labs(y = glue::glue("x-{sensor}"), x = "t(s)")
            y <- data[[sensor]] %>% 
                ggplot(aes_string(x = "t", y = "y")) + 
                geom_line(color = color) + 
                geom_hline(yintercept = 0, linetype = "twodash") +
                theme_minimal() + 
                labs(y = glue::glue("y-{sensor}"), x = "t(s)") 
            z <- data[[sensor]] %>% 
                ggplot(aes_string(x = "t", y = "z")) + 
                geom_line(color = color) + 
                geom_hline(yintercept = 0, linetype = "twodash") +
                theme_minimal() +
                labs(y = glue::glue("z-{sensor}"), x = "t(s)")
            if(stringr::str_detect(tolower(sensor), "^gravity")){
                x <- x + ylim(-1.2, 1.2)
                y <- y + ylim(-1.2, 1.2)
                z <- z + ylim(-1.2, 1.2)
            }else{
                x <- x +
                    ylim(-max(abs(data[[sensor]]$x)), 
                         max(abs(data[[sensor]]$x)))
                y <- y +
                    ylim(-max(abs(data[[sensor]]$y)), 
                         max(abs(data[[sensor]]$y)))
                z <- z +
                    ylim(-max(abs(data[[sensor]]$z)), 
                         max(abs(data[[sensor]]$z)))
            }
            plot <- x/y/z}) %>% 
            patchwork::wrap_plots(.)
        save_figure <- ggplot2::ggsave(
            output_filename, 
            plot, 
            device = "jpeg", 
            width = 10, 
            height = 6)
        return(output_filename)
    }, error = function(e){
        print(e)
        return(as.character(NA))
    })
    return(output_filename)
}

visualize_device_motion <- function(filepath){
    shape_sensor_data <- function(filepath){
        ts <- filepath %>%
            jsonlite::fromJSON(.) 
        sensor_list <- list(
            accel = tibble(t = ts$timestamp) %>%
                cbind(ts$userAcceleration) %>%
                dplyr::mutate(t = .$t - .$t[[1]]) %>%
                dplyr::select(t,x,y,z),
            rotation = tibble(t = ts$timestamp) %>%
                cbind(ts$rotationRate) %>%
                dplyr::mutate(t = .$t - .$t[[1]]) %>%
                dplyr::select(t,x,y,z),
            gravity =  tibble(t = ts$timestamp) %>%
                cbind(ts$gravity) %>%
                dplyr::mutate(t = .$t - .$t[[1]]) %>%
                dplyr::select(t,x,y,z)
        )
        return(sensor_list)
    }
    
    get_color_mapping <- function(cols_key){
        accel_col <- cols_key[stringr::str_detect(cols_key, "^accel")]
        rotation_col <- cols_key[stringr::str_detect(cols_key, "^gyro|^rotation")]
        gravity_col <- cols_key[stringr::str_detect(cols_key, "^gravity")]
        mapping_list <- stats::setNames(c(accel_col, rotation_col, gravity_col),
                                        c(accel_col, rotation_col, gravity_col))
        
        color_mapping <- purrr::map(mapping_list, function(detected_sensor){
            if(stringr::str_detect(tolower(detected_sensor), "^accel")){
                "lightskyblue3"
            }else if(stringr::str_detect(tolower(detected_sensor), "^gyro|^rotation")){
                "orange"
            }else{
                "palegreen3"
            }
        })
        return(color_mapping)
    }
    output_filename <- tryCatch({
        output_filename <- file.path(
            glue::glue(gsub(
                "\\.json$", "", 
                filepath), ".jpg"))
        data <-  shape_sensor_data(filepath)
        sensor_col_map <-  get_color_mapping(names(data))
        plot <- purrr::map(names(data), function(sensor){
            color <- sensor_col_map[[sensor]]
            x <- data[[sensor]] %>% 
                ggplot(aes_string(x = "t", y = "x")) + 
                geom_line(color = color) + 
                geom_hline(yintercept = 0, linetype = "twodash") +
                theme_minimal() +
                labs(y = glue::glue("x-{sensor}"), x = "t(s)")
            y <- data[[sensor]] %>% 
                ggplot(aes_string(x = "t", y = "y")) + 
                geom_line(color = color) + 
                geom_hline(yintercept = 0, linetype = "twodash") +
                theme_minimal() + 
                labs(y = glue::glue("y-{sensor}"), x = "t(s)") 
            z <- data[[sensor]] %>% 
                ggplot(aes_string(x = "t", y = "z")) + 
                geom_line(color = color) + 
                geom_hline(yintercept = 0, linetype = "twodash") +
                theme_minimal() +
                labs(y = glue::glue("z-{sensor}"), x = "t(s)")
            if(stringr::str_detect(tolower(sensor), "^gravity")){
                x <- x + ylim(-1.2, 1.2)
                y <- y + ylim(-1.2, 1.2)
                z <- z + ylim(-1.2, 1.2)
            }else{
                x <- x +
                    ylim(-max(abs(data[[sensor]]$x)), 
                         max(abs(data[[sensor]]$x)))
                y <- y +
                    ylim(-max(abs(data[[sensor]]$y)), 
                         max(abs(data[[sensor]]$y)))
                z <- z +
                    ylim(-max(abs(data[[sensor]]$z)), 
                         max(abs(data[[sensor]]$z)))
            }
            plot <- x/y/z}) %>% 
            patchwork::wrap_plots(.)
        save_figure <- ggplot2::ggsave(
            output_filename, 
            plot, 
            device = "jpeg", 
            width = 10, 
            height = 6)
        return(output_filename)
    }, error = function(e){
        print(e)
        return(as.character(NA))
    })
    return(output_filename)
}

visualize_photo <- function(filepath){
    if(tools::file_ext(filepath) == "png"){
        new_filepath <- sub('\\.png$', '.jpg', filepath)
        png_mat <- png::readPNG(filepath)
        jpeg::writeJPEG(png_mat, target = new_filepath, quality = 1)
        file.remove(filepath)
        return(new_filepath)
    }else{
        return(filepath)
    }
}