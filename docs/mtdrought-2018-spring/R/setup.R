knitr::opts_chunk$set(echo = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      results = "hold",
                      eval.after = "fig.cap",
                      fig.width = 4.45,
                      fig.height = 2.54,
                      fig.retina = 4, 
                      fig.align = "center", 
                      out.width = '100%', 
                      collapse=TRUE,
                      cache=TRUE
)

# install.packages("devtools")
# devtools::install_github("mt-climate-office/mcor")
# source("https://bioconductor.org/biocLite.R")
# devtools::install_github("ecohealthalliance/fasterize")
# biocLite("rhdf5")

## Load all packages
all_packages <- c("mcor", "thredds",# The Core MCO package
                  "FedData", "smapr", "rhdf5", # Package for data aquisition
                  "sf", "raster", "rgeos", "fasterize", "velox", "lwgeom",# Packages for spatial processing
                  "magrittr", "tidyverse", "purrrlyr", # Packages for tidy code
                  "RColorBrewer", "htmlwidgets", "htmltools", "leaflet", "plotly",
                  "bibtex", "knitcitations", "kableExtra") # Plotting and rmarkdown

# devtools::install_bioc("rhdf5")
# purrr::walk(all_packages, devtools::install_cran, character.only = TRUE)
purrr::walk(all_packages, library, character.only = TRUE)

# Load other useful functions
list.files("../R", full.names = T) %>%
  purrr::walk(source)

rasterOptions(maxmemory = 1e+09)
