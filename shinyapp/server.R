

# ------------------------------------------------------------------------------
# This is the server definition of the Shiny web application
# ------------------------------------------------------------------------------

# -- Server logic
shinyServer(
    function(input, output){

      
      cat("-------------------------------------------------- \n")
      cat("Starting application server \n")
      cat("-------------------------------------------------- \n")
      
      
      # -- declare r communication object
      r <- reactiveValues()
      
      
      # ------------------------------------------------------------------------
      # Project Management
      # ------------------------------------------------------------------------
      
      # -- call module servers
      projectManager_Server(id = "project", r = r, path = path)
      taskGroupManager_Server(id = "taskGroup", r = r, path = path)
      taskManager_Server(id = "task", r = r, path = path)
      timesheetManager_Server(id = "time", r = r, path = path)
      
      
      cat("-------------------------------------------------- \n")
      cat("Application server ready \n")
      cat("-------------------------------------------------- \n")
        
    }
)
