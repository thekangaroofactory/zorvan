

# -------------------------------------
# [TASKGROUP] functions 
# -------------------------------------

# -- get id
getTaskGroupID <- function(r, index = NULL, name = NULL, project = NULL){
  
  # select by index
  if(!is.null(index)){
    
    id <- r$taskGroups()[index, ]$id
    
  }
  
  # select by name
  if(!is.null(name)){
    
    id <- r$taskGroups()[r$taskGroups()$name == name, ]$id
    
    # check non unique
    if(length(id) > 1){
      
      id <- r$taskGroups()[r$taskGroups()$project.id == project & r$taskGroups()$name == name, ]$id
      
    }
    
  }
  
  # return
  id
  
}


# -- replace id by name
replaceTaskGroupName <- function(r, input = NULL){
  
  # get table
  taskgroup <- r$taskGroups()
  
  # replace
  input$taskgroup <- taskgroup$name[match(input$task.group.id, taskgroup$id)]
  
  # return
  input
  
}


# -- get name by id
getTaskGroupName <- function(r, id = NULL){
  
  # get value
  r$taskGroups()$name[match(id, r$taskGroups()$id)]
  
}
