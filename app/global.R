################################################################################
# Entrypoint of the Shiny app
#
# Author: Stefan Schliebs
# Created: 2020-03-04 09:30:40
################################################################################


library(shiny)
library(dplyr)
library(tidyr)
library(purrr)
library(echarts4r)
library(glue)
library(leaflet)
library(shinyWidgets)
library(sf)
library(shinyjs)



# Utilities ---------------------------------------------------------------

source("utils/ui-utils.R")



# Modules -----------------------------------------------------------------

source("modules/mod_count_icon.R")
source("modules/mod_commute_explorer.R")
