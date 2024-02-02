# --------------------------------------------------------------------------------
# Shiny module: projects
# --------------------------------------------------------------------------------

# -------------------------------------
# [TABLE]
# -------------------------------------

# -- Project table UI
# projectTable_UI <- function(id)
# {
# 
#   # namespace
#   ns <- NS(id)
# 
#   # table
#   DTOutput(ns("projectTable"),
#            width = 600)
# 
# }


# -------------------------------------
# [SET ACTIVE PROJECT]
# -------------------------------------

# -- Select Project
projectSelector_Input <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("project_selector"))
  
}


# -------------------------------------
# [ACTION BUTTONS]
# -------------------------------------

# -- Action buttons
projectActions_BTN <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("action_buttons"))
  
}