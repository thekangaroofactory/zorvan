
library(dplyr)
library(ggplot2)
library(kgraph)

# -------------------------------------
# [helper] functions 
# -------------------------------------

# -- get id
getTaskID <- function(r, project = NULL, taskgroup = NULL, name = NULL){
  
  id <- r$tasks()[r$tasks()$project.id == project & r$tasks()$task.group.id == taskgroup & r$tasks()$name == name, ]$id
  
}


# -- replace id by name
replaceTaskName <- function(r, input = NULL){
  
  # get table
  task <- r$tasks()
  
  # replace
  input$task <- task$name[match(input$task.id, task$id)]
  
  # return
  input
  
}


# -- get name by id
getTaskName <- function(r, id = NULL){
  
  r$tasks()$name[match(id, r$tasks()$id)]
  
}


# -------------------------------------
# [PLOT] functions
# -------------------------------------

# -- Task Status
getTaskStatusPlot <- function(tasks){
  
  # prepare data
  tasks <- tasks[tasks$status != "obsolete", ] %>%
    group_by(status) %>%
    summarise(count = n(), progress = mean(progress))
  
  # rename to fit function requirements
  names(tasks) <- c("category", "nb", "progress")
  
  # make plot
  k_exploded_radar(data = tasks)
  
}


# -- On Time Delivery Status
getOnTimeDeliveryPlot <- function(tasks){
  
  # filter tasks status
  tasks <- tasks[tasks$status == "Done", ]
  
  # check size
  if(dim(tasks)[1] > 0){
    
    # build ontime status
    tasks$ontime <- tasks$date.done <= tasks$date.planned
    
    # prepare data
    tasks <- tasks %>%
      group_by(ontime) %>%
      summarise(count = n())
    
    # rename to fit donut function requirements
    names(tasks) <- c("category", "count")
    
    # make plot
    donut(tasks, 1)
    
  } else {NULL}
  
}


#' Task Progress Plot
#'
#' @param task_list a data.frame constaining the list of task to build the plot.
#'
#' @return a plot (output of ggplot2)
#' @export
#'
#' @examples getTaskProgressPlot(task_list = my_task_df)

getTaskHistoryPlot <- function(task_list){
  
  # date task created
  create_df <- task_list %>%
    group_by(date = date.create) %>%
    summarise(created = n(), planned = 0, done = 0)
  
  # date task planned
  plan_df <- task_list %>%
    group_by(date = date.planned) %>%
    summarise(created = 0, planned = n(), done = 0)
  
  # date task done
  done_df <- task_list %>%
    group_by(date = date.done) %>%
    summarise(created = 0, planned = 0, done = n())
  
  # merge and order by date
  data <- rbind(create_df, plan_df, done_df)
  data <- data[order(data$date), ]
  
  # replace count by cumsum
  data <- data %>%
    summarise(date = date, cum_created = cumsum(created), cum_planned = cumsum(planned), cum_done = cumsum(done))
  
  data <- data[complete.cases(data), ]
  
  # ------------------------------- DEBUG
  # task_list <<- task_list
  # create_df <<- create_df
  # plan_df <<- plan_df
  # done_df <<- done_df
  # debug_data <<- data
  # ------------------------------- DEBUG
  
  
  # make plot
  p <- ggplot() +
    geom_ribbon(data = data, aes(x = date, ymin = 0, ymax = cum_created, colour = "Created"), color = "black", fill = "#E69F00", alpha = 0.1, linetype = "dashed") +
    geom_line(data = data, aes(x = date, y = cum_planned, colour = "Planned"), color = "black", fill = "#E69F00", alpha = 0.3, linetype = "dashed") +
    geom_ribbon(data = data, aes(x = date, ymin = 0, ymax = cum_done, colour = "Done"), color = "black", fill = "#E69F00", alpha = 0.3, linetype = "dashed")
  
  
  # apply theme
  p <- p +  theme(axis.title.x = element_blank(),
                  axis.title.y = element_blank())
  
}


