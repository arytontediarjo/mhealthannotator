# mhealthannotator
Mhealthannotator is a RShiny-as-a-service App for visualizing Files in Synapse Tables used @Sage Bionetworks. 

## About
This app's purpose is to help researchers/engineers/analyst be able to spin up <b> Image Label Annotations </b> App more seamlessly through using RShiny as its core UI/UX.

It will help interact with Synapse by providing: 

a. Configuration files: 
- Containing information to interact with synapse
- Containing information to the RShiny App UI/UX

b. Visuallization functions: Custom function you want to use to visualize your data

## Installation
Current package is not distributed via CRAN but will be installable through Github as we are doing further testing and future use-cases.

To install the development from GitHub, run:
``` r
devtools::install_github("Sage-Bionetworks/mhealthannotator")
```
Notes on Installation:
All functionalities in mhealthannotator use reticulate and the [Synapse Python
client](https://pypi.org/project/synapseclient/). You can set up your environment by having a anaconda environment or a virtual environment set up with Synapseclient. Because mhealthannotator uses reticulate, it is not compatible with the [synapser](https://r-docs.synapse.org/) package..

## How To Run:
1. Configure Python Environment:
As we require to use synapse python client, python environment needed to be configured:
Resources can be found here: 
- Reticulate: https://rstudio.github.io/reticulate/
- Renv (Suggested): https://rstudio.github.io/renv/articles/python.html

2. Build Configuration
After your python environment is set up, you will be able to parse in your configuration and visualization function.

- Config Template
- Function Template

Notes: Example configuration can be found in this package inst/* and visualization on R/visualizer.R

3. Run Shiny App
```r
mhealthannotator::run_app(config = <PATH_TO_CONFIG_FILE>, funs = <PLOT_FUNCTION>)
```

## Deployment:

### a. Shinyapps.io (In-Development)

### b. Shiny Server Pro

[Resources to connect to Sage SciComp Shiny Server Instance](https://sagebionetworks.jira.com/wiki/spaces/SageShinyServer/pages/75497489/Shiny+Server)

1. Git Clone this Repo on your server directory
```zsh
git clone https://github.com/Sage-Bionetworks/mhealthannotator.git
```
2. Repeat Step 1 and 2 of `How to Run`
3. Modify app.R based on your parameter of Step 3 of `How to Run`


