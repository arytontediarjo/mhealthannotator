# Sage-Bionetworks mHealth Data Annotator

### About
This R-Shiny Package is a tool that will be used to parse in <b>Synapse Table Files</b> and visualize it in an Shiny App. User will have the freedom to choose where they would like to store their annotations and what kind of input buttons they would like to have for the app.

### Installation
Current package is not distributed via CRAN but will be installable through Github as we are doing further testing and future use-cases.

To install the development from GitHub, run:
``` r
devtools::install_github("Sage-Bionetworks/mhealthannotator")
```
Notes on Installation:
All functionalities in mhealthannotator use reticulate and the [Synapse Python
client](https://pypi.org/project/synapseclient/). You can set up your environment by having a anaconda environment or a virtual environment set up with Synapseclient. Because mhealthannotator uses reticulate, it is not compatible with the [synapser](https://r-docs.synapse.org/) package..

### Adding Python using R `renv` package (Suggested):
```r
install.packages("renv")
renv::use_python(<your python environment based on `requirements.txt`>)
```
### Running your Shiny App
```r
mhealthannotator::run_app(config = "config/psorcast_plaque_config.yml", funs = visualize_photo)
```
