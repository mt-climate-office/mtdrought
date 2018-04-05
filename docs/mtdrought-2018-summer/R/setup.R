library(magrittr)

expansion <- list(left = 0.2,
                  right = 0.01,
                  bottom = 0.01,
                  top = 0.01)

mt_plot_ratio <- 
  mcor::mt_state %>%
  sf::st_bbox()  %>%
  as.list() %$% {
    ((ymax - ymin) +
       ((ymax - ymin) * expansion$bottom) + 
       ((ymax - ymin)*expansion$top)) / 
      ((xmax - xmin) + 
         ((xmax - xmin)*expansion$right) + 
         ((xmax - xmin)*expansion$left))
  }

fig_height <- 2.5
fig_width <- fig_height / mt_plot_ratio

knitr::opts_chunk$set(echo = FALSE,
                      message = FALSE,
                      warning = FALSE,
                      results = "hold",
                      eval.after = "fig.cap",
                      fig.width = fig_width,
                      fig.height = fig_height,
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
                  "sf", "raster", "rgeos", "fasterize", "velox", "spex", "lwgeom",# Packages for spatial processing
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

save_mt_map <- function(x, 
                        name,
                        width = fig_width,
                        height = fig_height)
{
  x %>%
    ggplot2::ggsave(name,
                    plot = .,
                    device = "pdf",
                    path = "./figures/",
                    width = width,
                    height = height
    )
}

