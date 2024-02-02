

# --------------------------------------------------------------------------------
# Shiny module: taskGroups
# --------------------------------------------------------------------------------

# -------------------------------------
# [TABLE]
# -------------------------------------

# -- UI
taskGroupsTable_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  DTOutput(ns("taskGroupTable"), width = 600)
  
}


# -------------------------------------
# [FILTER]
# -------------------------------------

# -- Input
taskgroupFilter_Input <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("taskgroup_filter"))
  
}


# -------------------------------------
# [ACTION BUTTONS]
# -------------------------------------

# -- Buttons
taskGroup_BTN <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("action_buttons"))
  
}
