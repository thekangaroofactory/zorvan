
# --------------------------------------------------------------------------------
# This is the server definition of the Shiny web application
# --------------------------------------------------------------------------------

# -- Library

library(shiny)
library(DT)


# -- Init env
#source("./init_env.R")

# -- Declare path
path <- list(project = "../",
             script = "../shinyapp/src",
             data = "../data")


# -- Source scripts
cat("[SERVER] Source code... \n")
for (nm in list.files(path$script, full.names = TRUE, recursive = TRUE, include.dirs = FALSE))
{
  cat("Loading from: ", nm, "\n")
  source(nm)
}
rm(nm)


# -- dependencies
source("~/Work/R/Library/Time and date/getTimestamp.R")


# -- Define server logic

shinyServer(
    function(input, output){
      
      # *********************************************************************************
      # DEBUG
      
      cat("[*** DEBUG ***] SERVER RESTART Source code... [*** TO BE REMOVED ***] \n")
      for (nm in list.files(path$script, full.names = TRUE, recursive = TRUE, include.dirs = FALSE))
      {
        cat("Loading from: ", nm, "\n")
        source(nm)
      }
      rm(nm)
      # 
      # # -- dependencies
      # source("~/Work/Projects/Zorvan/Zorvan/src/donut.R")
      
      
      # DEBUG
      # *********************************************************************************
      
      cat("-------------------------------------------------- \n")
      cat("Starting application server \n")
      cat("-------------------------------------------------- \n")
      
      # declare r communication object
      
      # -------------------------------------
      # - projects: A data.frame of project items
      # - active_project: A vector containing the current (selected) project id
      # - taskGroups: A data.frame of taskGroup itemps
      # - selectedTaskGroup: A vector containing the current (selected) taskGroup id
      # - selectedTask: A vector containing the current (selected) task id
      # - weight: A data.fram of weight items
      # -------------------------------------
      
      r <- reactiveValues()
      
      
      # -------------------------------------
      # Project Management
      # -------------------------------------
      
      projectManager_Server(id = "project", r = r, path = path)
      taskGroupManager_Server(id = "taskGroup", r = r, path = path)
      taskManager_Server(id = "task", r = r, path = path)
      timesheetManager_Server(id = "time", r = r, path = path)
      
      
      cat("-------------------------------------------------- \n")
      cat("Application server ready \n")
      cat("-------------------------------------------------- \n")
        
    }
)
