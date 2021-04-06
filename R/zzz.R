synapseclient <- NULL
synapseutils <- NULL

.onLoad <- function(libname, pkgname) {
    synapseclient <<- reticulate::import("synapseclient")
    synapseutils <<- reticulate::import("synapseutils")
}

