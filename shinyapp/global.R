

# ------------------------------------------------------------------------------
# Init. environment (shared with both ui.R & server.R)
# ------------------------------------------------------------------------------

# -- Dependencies
library(shiny)
library(shinydashboard)
library(DT)
library(kgraph)


# ------------------------------------------------------------------------------
# Declare shared objects
# ------------------------------------------------------------------------------

# -- Declare path
path <- list(code = "../shinyapp/src",
             data = "../data")


# ------------------------------------------------------------------------------
# Source code
# ------------------------------------------------------------------------------

# -- Source modules & scripts
cat("Source code... \n")
for (nm in list.files(path$code, full.names = TRUE, recursive = TRUE, include.dirs = FALSE))
{
  cat("-- Loading from: ", nm, "\n")
  source(nm)
}
rm(nm)
