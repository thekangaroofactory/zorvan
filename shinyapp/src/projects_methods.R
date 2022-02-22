

# ------------------------------------------------------------------------------
# [PROJECT] Functions
# ------------------------------------------------------------------------------

# -- Replace project id by project name
replaceProjectName <- function(r, input = NULL){
  
  # get project table
  projects <- r$projects()
  
  # replace
  input$project <- projects$name[match(input$project.id, projects$id)]
  
  # return
  input
  
}

