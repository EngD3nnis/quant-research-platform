# ============================================================
# Launch the Quant Research Platform Shiny Dashboard
#
# From R or RStudio console, with working directory set to the
# project root:
#   source("run.R")
#
# Or directly from the terminal:
#   Rscript run.R
# ============================================================

if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

setwd(here::here())

# Install any missing packages
source(here::here("install_packages.R"), local = TRUE)

# Launch
message("Starting Quant Research Platform at http://127.0.0.1:3838")
shiny::runApp(
  appDir         = here::here("shiny-dashboard"),
  launch.browser = TRUE,
  host           = "127.0.0.1",
  port           = 3838
)
