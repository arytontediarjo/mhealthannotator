synapseclient <- NULL
synapseutils <- NULL

.onLoad <- function(libname, pkgname) {
    synapseclient <<- reticulate::import("synapseclient", delay_load = TRUE)
    synapseutils <<- reticulate::import("synapseutils", delay_load = TRUE)
}

