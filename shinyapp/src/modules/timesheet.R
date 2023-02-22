
# --------------------------------------------------------------------------------
# Shiny module: timesheet
# --------------------------------------------------------------------------------

# -- Library



# -------------------------------------
# [TABLE]
# -------------------------------------

# -- UI:
timesheetTable_project_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  DTOutput(ns("timesheetTable_project"))
  
}


# -------------------------------------
# [FILTER] 
# -------------------------------------

# -- UI:
timeSheet_filters_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("filters"), inline = TRUE)
  
}


# -------------------------------------
# [ACTION BUTTONS]
# -------------------------------------

# -- UI:
timeSheet_buttons_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("action_buttons"))
  
}


# -------------------------------------
# [KPI]
# -------------------------------------

# -- UI: effort box
kpiEffort_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("kpi_effort"))
  
}


# -- UI: last effort box
kpiLastEffort_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("kpi_lastEffort"))
  
}


# -- Plot: workload
kpiWorkload_PLOT <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  plotOutput(ns("kpi_Workload"), height = "auto")
  
}


# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

timesheetManager_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -------------------------------------
    # INIT
    # -------------------------------------
    
    # get namespace
    ns <- session$ns
    
    # -- declare objects
    filename <- "timesheet.csv"
    cols <- c(id = "double",
              date = "POSIXct",
              project.id = "double",
              task.group.id = "double",
              task.id = "double",
              time = "double",
              description = "character")
    
    # -- declare communication connectors
    # >> none
    
    # -- declare internal views
    project_timesheet <- NULL
    filtered_timesheet <- NULL
    
    # -- declare internal objects
    selectedTime  <- reactiveVal(NULL)
    
    # -- in table selection
    previousSelection <- NULL
    previousPage <- 0
    
    
    # --------------------------------------------------------------------------
    # [DATA & VIEWS] Update reactive value & save
    # --------------------------------------------------------------------------
    
    # -- load data
    #r$timesheet <- reactiveVal(read.data(path$data, filename, cols)) << migrate to kfiles
    r$timesheet <- reactiveVal(kfiles::read_data(file = filename,
                                                 path = path$data,
                                                 colClasses = cols))
    
    
    # -- update and save data
    updateAndSave <- function(timesheet){
      
      # -- cache table row and page (to keep selection upon edit)
      previousSelection <<- input$timesheetTable_project_rows_selected
      previousPage <<- input$timesheetTable_project_rows_current[1] -1
      
      # store
      r$timesheet(timesheet)
      
      # save
      #write.data(timesheet, path$data, filename) << migrate to kfiles
      kfiles::write_data(data = timesheet, 
                         file = filename, 
                         path = path$data)
      
      # log & notify
      cat("[TIME] Item list saved \n")
      showNotification("Timesheet saved", type = "message")
      
    }
    
    
    # -- project view
    project_timesheet <- reactive({
      
      # log
      cat("[TIME] Apply active project \n")
      
      # check
      if(!is.null(r$active_project())){
        
        # apply filter
        r$timesheet()[r$timesheet()$project.id == r$active_project(), ]
        
      } else {r$timesheet()}
      
    })
    
    
    # -- filtered view
    filtered_timesheet <- reactive({
      
      # log
      cat("[TIME] Apply active filters \n")
      
      # check TaskGroup filter
      if(!is.null(r$filter_TaskGroup())){
        
        # check Task filter
        if(!is.null(r$filter_Task())){
          
          # apply both TaskGroup & Task filters
          project_timesheet()[project_timesheet()$task.group.id == r$filter_TaskGroup() & project_timesheet()$task.id == r$filter_Task(), ]
          
        } else {
          
          # apply only TaskGroup filter
          project_timesheet()[project_timesheet()$task.group.id == r$filter_TaskGroup(), ]
          
        }
        
      } else {project_timesheet()}
      
    })
    
    
    # -- helper: build view
    getTableView <- function(timesheet = NULL){
      
      # replace ids by names
      timesheet <- replaceProjectName(r, input = timesheet)
      timesheet <- replaceTaskGroupName(r, input = timesheet)
      timesheet <- replaceTaskName(r, input = timesheet)
      
      # format
      timesheet$date <- as.Date(timesheet$date)
      
      # hide columns
      timesheet <- subset(timesheet, select = -c(1,3,4,5))
      
      # reorder & rename
      timesheet <- timesheet[c(4,5,6,1,2,3)]
      names(timesheet) <- c("Project", "TaskGroup", "Task", "Date", "Time", "Description")
      
      # return
      timesheet
      
    }
    
    
    # -------------------------------------
    # [FILTERS]
    # -------------------------------------
    
    
    
    # -------------------------------------
    # [TABLE]
    # -------------------------------------
    
    # -- UI: Table
    output$timesheetTable_project <- renderDT(getTableView(filtered_timesheet()),
                                              rownames = FALSE,
                                              options = list(lengthMenu = c(10, 15, 20), pageLength = 15, displayStart = previousPage, dom = "ltp"),
                                              selection = list(mode = 'single', target = "row", selected = previousSelection))
    
    
    # -- Observe: In table selection
    observeEvent(input$timesheetTable_project_rows_selected, ignoreNULL = FALSE, {
      
      # check input
      if(!is.null(input$timesheetTable_project_rows_selected) & length(input$timesheetTable_project_rows_selected) != 0){
        
        # get id and name
        id <- filtered_timesheet()[input$timesheetTable_project_rows_selected, ]$id
        
        # trace
        cat("[TIME] In table selection: id =", as.character(id), "\n")
        
      } else {id <- NULL}
      
      # cache id
      selectedTime(id)
      
    })
    
    
    # -------------------------------------
    # [MODAL]
    # -------------------------------------
    
    # -- helper: define item inputs
    getItemModal <- function(mode = "create"){
      
      # manage create / update mode
      if(mode == "update"){
        
        # get target item
        target_item <- r$timesheet()[r$timesheet()$id == selectedTime(), ]
        task_name <- getTaskName(r, target_item$id)
        
        # set defaults
        title = paste("Update time log:", target_item$id)
        date_value <- as.Date(target_item$date)
        taskGroup_selected <- getTaskGroupName(r, target_item$task.group.id)
        task_choices <- task_name
        task_selected <- task_name
        time_value <- target_item$time
        description_value <- target_item$description
        
        # button
        btn_id <- "btn_confirm_update"
        btn_label <- "Update"
        
      } else {
        
        # set defaults
        title = paste("[", r$projects()[r$projects()$id == r$active_project(), ]$name, "] New time log")
        date_value <- as.Date(Sys.Date())
        taskGroup_selected <- NULL
        task_choices <- NULL
        task_selected <- NULL
        time_value <- 0
        description_value <- ""
        
        # button
        btn_id <- "btn_confirm_new"
        btn_label <- "Create"
        
      }
      
      # -- build dialog
      modalDialog(
        
        title = title,
        
        # inputs
        dateInput(inputId = ns("date"),
                  label = "Date",
                  value = date_value,
                  format = "yyyy-mm-dd",
                  weekstart = 1),
        
        selectInput(inputId = ns("taskGroup"),
                    label = "TaskGroup",
                    choices = r$taskGroups()[r$taskGroups()$project.id == r$active_project(), ]$name,
                    selected = taskGroup_selected,
                    multiple = FALSE),
        
        selectInput(inputId = ns("task"),
                    label = "Task",
                    choices = task_choices,
                    selected = task_selected,
                    multiple = FALSE),
        
        numericInput(inputId = ns("time"),
                     label = "Time",
                     value = time_value,
                     min = 0,
                     step = 0.5),
        
        textInput(inputId = ns("description"), 
                  label = "Description",
                  value = description_value,
                  placeholder = 'Type something'),
        
        # footer
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns(btn_id), btn_label)))
      
    }
    
    
    # -- Observe: track taskGroup change in modal
    observeEvent(input$taskGroup, {
      
      cat("[MODAL] Update task input choices \n")
      
      # get taskGroup id
      id <- getTaskGroupID(r, name = input$taskGroup, project = r$active_project())
      
      # update task input
      updateSelectInput(
        session = getDefaultReactiveDomain(),
        inputId = "task",
        choices = r$tasks()[r$tasks()$task.group.id == id, ]$name)
      
    })
    
    
    
    # -------------------------------------
    # [ACTION BUTTONS]
    # -------------------------------------
    
    # -- define action buttons
    output$action_buttons <- renderUI(tagList(
      
      # Button: always ON
      actionButton(ns("btn_new"), "New"),
      
      # Button: when row selected
      if(!is.null(selectedTime())){
        
        tagList(
          
          # Button: delete
          actionButton(ns("btn_delete"), "Delete"),
          
          # Button: update
          actionButton(ns("btn_update"), "Update"),
          
          # Button: -
          actionButton(ns("btn_timeRemove"), "Time -"),
          
          # Button: +
          actionButton(ns("btn_timeAdd"), "Time +")
          
        )}))
    
    
    # -- Observe: new
    observeEvent(input$btn_new, {
      
      # display form
      showModal(getItemModal())
      
    })
    
    
    # -- Observe: confirm new (modal)
    observeEvent(input$btn_confirm_new, {
      
      # required
      req(input$date,
          input$taskGroup,
          input$task,
          input$time)
      
      # close form
      removeModal()
      
      # log
      cat("[TIME] Create new item \n")
      
      # get ids
      id <- getTimestamp()
      project_id <- r$active_project()
      taskgroup_id <- getTaskGroupID(r, name = input$taskGroup, project = project_id)
      task_id <- getTaskID(r, project = project_id, taskgroup = taskgroup_id, name = input$task)
      
      # new object
      new_item <- data.frame(id = id,
                             date = as.POSIXct(input$date),
                             project.id = project_id,
                             task.group.id = taskgroup_id,
                             task.id = task_id,
                             time = input$time,
                             description = input$description)
      
      # merge & update
      timesheet <- rbind(r$timesheet(), new_item)
      updateAndSave(timesheet)
      
    })
    
    
    # -- Observe: click update button
    observeEvent(input$btn_update, {
      
      # display form
      showModal(getItemModal(mode = "update"))
      
    })
    
    
    # -- Observe: confirm new (modal)
    observeEvent(input$btn_confirm_update, {
      
      # required
      req(input$date,
          input$taskGroup,
          input$task,
          input$time)
      
      # close form
      removeModal()
      
      # log
      cat("[TIME] Update item \n")
      
      # get item
      target_item <- r$timesheet()[r$timesheet()$id == selectedTime(), ]
      
      # transform input
      taskgroup_id <- getTaskGroupID(r, name = input$taskGroup, project = target_item$project.id)
      task_id <- getTaskID(r, project = target_item$project.id, taskgroup = taskgroup_id, name = input$task)
      
      # update object
      target_item$date <- as.POSIXct(input$date)
      target_item$task.group.id <- taskgroup_id
      target_item$task.id <- task_id
      target_item$time <- input$time
      target_item$description <- input$description
      
      # get list and replace item
      timesheet <- r$timesheet()
      timesheet[timesheet$id == selectedTime(), ] <- target_item
      
      # store & save
      updateAndSave(timesheet)
      
    })
    
    
    # -- Button: Delete
    observeEvent(input$btn_delete, {
      
      # log
      cat("[TIME] Delete selected item \n")
      
      # -- 2 steps delete, ask for confirmation
      showModal(modalDialog(
        
        p("Do you confirm deletion?"),
        
        title = "Delete time log",
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete"), "Confirm delete"))))
      
    })
    
    
    # -- Button: Confirm delete
    observeEvent(input$confirm_delete, {
      
      # log
      cat("[TIME] Delete confirmed, id =", selectedTime(), "\n")
      
      # close modal
      removeModal()
      
      # remove item
      timesheet <- r$timesheet()[r$timesheet()$id != selectedTime(), ]
      
      # store & save
      updateAndSave(timesheet)
      
    })
    
    
    # -- Button: time remove
    observeEvent(input$btn_timeRemove, {
      
      # log
      cat("[TIME] Remove time, id =", selectedTime(), "\n")
      
      # get item
      target_item <- r$timesheet()[r$timesheet()$id == selectedTime(), ]
      
      # check
      if(target_item$time > 0.5){
        
        # update object
        target_item$time <- target_item$time - 0.5
        
        # get list and replace item
        timesheet <- r$timesheet()
        timesheet[timesheet$id == selectedTime(), ] <- target_item
        
        # store & save
        updateAndSave(timesheet)
        
      } else {
        
        # notify
        showNotification("[TIME] Can't remove time (do delete instead).", type = "warning")}
      
    })
    
    
    # -- Button: time remove
    observeEvent(input$btn_timeAdd, {
      
      # log
      cat("[TIME] Add time, id =", selectedTime(), "\n")
      
      # get item
      target_item <- r$timesheet()[r$timesheet()$id == selectedTime(), ]
      
      # update object
      target_item$time <- target_item$time + 0.5
      
      # get list and replace item
      timesheet <- r$timesheet()
      timesheet[timesheet$id == selectedTime(), ] <- target_item
      
      # store & save
      updateAndSave(timesheet)
      
    })
    
    
    # -------------------------------------
    # [KPI] Section
    # -------------------------------------
    
    # -- Box: effort
    output$kpi_effort <- renderUI(expr = {
      
      # compute value
      effort <- sum(filtered_timesheet()$time)
      valueBox(paste(effort, "h"), "Effort", color = "light-blue", width = 12)})
    
    
    # -- Box: last effort
    output$kpi_lastEffort <- renderUI(expr = {
      
      # compute value
      last_effort <- as.Date(max(filtered_timesheet()$date))
      valueBox(last_effort, "Last effort", color = "light-blue", width = 12)})
    
    
    # -- Plot: workload
    output$kpi_Workload <- renderPlot(getWorkloadPlot(filtered_timesheet()), width = "auto", height = 300, bg = "transparent")
    
    
  })
}

