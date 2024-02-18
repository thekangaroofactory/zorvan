

# -- function definition
taskgroup_view <- function(taskgroups){
  
  # -- define
  cols_to_hide <- c("id", "project.id", "before", "after")
  cols_display_name <- c("Name")
  
  # -- filter
  taskgroups <- taskgroups[!names(taskgroups) %in% cols_to_hide]
  colnames(taskgroups) <- cols_display_name
  
  # -- return
  taskgroups
  
}