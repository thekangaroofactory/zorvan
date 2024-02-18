
# --------------------------------------------------------------------------------
# Shiny module: tasks
# --------------------------------------------------------------------------------

# -- Library


# -------------------------------------
# [TABLE]
# -------------------------------------

# -- UI:
taskTable_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  DTOutput(ns("taskTable"))
  
}


# -------------------------------------
# [FILTERS]
# -------------------------------------

# -- Input: select task
taskFilter_INPUT <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("task_filter"))
  
}


# -------------------------------------
# [KPI]
# -------------------------------------

# -- KPI: progress box
kpiProgress_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("kpi_progress"))
  
}


# -- KPI: overdue box
kpiOverdue_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("kpi_overdue"))
  
}


# -- KPI: overdue box
kpiNextTD_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("kpi_nextTD"))
  
}


# -- Plot: task status
kpiTaskStatus_PLOT <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  plotOutput(ns("kpi_taskStatus"), height = "auto")
  
}


# -- Plot: on time delivery
kpiOnTimeStatus_PLOT <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  plotOutput(ns("kpi_onTimeStatus"), height = "auto")
  
}


# -- Plot: task history
kpiTaskHistory_PLOT <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  plotOutput(ns("kpi_taskHistory"), height = "auto")
  
}


# -------------------------------------
# [ACTION BUTTONS]
# -------------------------------------

# -- Task buttons
taskButtons_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("action_buttons"))
  
}


# -------------------------------------
# [TODO]
# -------------------------------------

taskTodoList_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  DTOutput(ns("todo.list"))
  
}


# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

taskManager_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {
    
    # -------------------------------------
    # Declare objects
    # -------------------------------------
    
    # get namespace
    ns <- session$ns
    
    # -- declare objects
    filename <- "tasks.csv"
    cols <- c(id = "double",
              name = "character",
              date.create = "POSIXct",
              date.planned = "POSIXct",
              date.done = "POSIXct",
              deplayed = "integer",
              project.id = "double",
              task.group.id = "double",
              before = "character",
              after = "character",
              progress = "integer",
              status = "character",
              milestone.id = "double",
              note = "character")
    
    # -- declare communication connectors
    r$filter_Task  <- reactiveVal(NULL)
    
    # -- declare internal objects
    selectedTask <- reactiveVal(NULL)
    
    # -- declare internal views
    project_tasks <- NULL
    taskGroup_tasks <- NULL
    filtered_tasks <- NULL
    
    # -- plots (to avoid blank outputs...)
    #p_taskStatus <- reactiveVal(NULL)
    #p_onTimeStatus <- NULL
    #p_taskHistory  <- NULL
    
    
    # -------------------------------------
    # [DATA & VIEWS]
    # -------------------------------------
    
    # -- load data
    # r$tasks <- reactiveVal(read.data(path$data, filename, cols)) << migrate to kfiles
    r$tasks <- reactiveVal(kfiles::read_data(file = filename,
                                             path = path$data,
                                             colClasses = cols))
    
    # -- store & save
    storeAndSave <- function(new_item_list){
      
      # cache
      r$tasks(new_item_list)
      
      # save
      # write.data(r$tasks(), path$data, filename) << migrate to kfiles
      kfiles::write_data(data = r$tasks(), 
                         file = filename, 
                         path = path$data)
      
      # log & notify
      cat("[TASK] Item list saved \n")
      showNotification("Task saved.", type = "message")
      
    }
    
    
    # -- project view
    project_tasks <- reactive({
      
      # check
      if(!is.null(r$active_project())){
        
        # apply filter
        cat("[TASK] Apply active project \n")
        r$tasks()[r$tasks()$project.id == r$active_project(), ]
        
      } else {r$tasks()}
      
    })
    
    
    # -- taskgroup view
    taskGroup_tasks <- reactive({
      
      # log
      cat("[TASK] Apply taskgroup filter \n")
      
      # check
      if(!is.null(r$filter_TaskGroup())){
        
        # apply filter
        project_tasks()[project_tasks()$task.group.id == r$filter_TaskGroup(), ]
        
      } else {
        
        r$filter_Task(NULL)
        project_tasks()
        
      }
      
    })
    
    
    # -- filtered view
    filtered_tasks <- reactive({
      
      # log
      cat("[TASK] Apply task filter \n")
      
      # check Task filter
      if(!is.null(r$filter_Task())){
        
        # apply task filters
        taskGroup_tasks()[taskGroup_tasks()$id == r$filter_Task(), ]
        
      } else {taskGroup_tasks()}
      
    })
    
    
    # -------------------------------------
    # [ITEM TABLE]
    # -------------------------------------
    
    # -- UI: Table
    output$taskTable <- renderDT(filtered_tasks()[-c(1, 7, 8)],
                                 rownames = FALSE,
                                 options = list(lengthMenu = c(5, 10, 15), pageLength = 5, dom = "ltp"),
                                 selection = list(mode = 'single', target = "row", selected = NULL))
    
    
    # -- Observe: In table selection
    observeEvent(input$taskTable_rows_selected, ignoreNULL = FALSE, {
      
      # cache row index (to keep selection upon edit)
      #selected.row(input$taskTable_rows_selected)
      
      # check
      if(!is.null(input$taskTable_rows_selected) & length(input$taskTable_rows_selected) != 0){
        
        # get id and name
        id <- filtered_tasks()[input$taskTable_rows_selected, ]$id
        name <- filtered_tasks()[input$taskTable_rows_selected, ]$name
        
        # log
        cat("[TASK] In table selection: id =", as.character(id), "name =", name, "\n")
        
      } else {id <- NULL}
      
      # cache id
      selectedTask(id)
      
    })
    
    
    # -------------------------------------
    # [FILTERS]
    # -------------------------------------
    
    # -- UI: build input
    output$task_filter <- renderUI(expr = {
      
      # check TaskGroup filter status (hide if NULL)
      if(!is.null(r$filter_TaskGroup())){
        
        selectizeInput(inputId = ns("filter_task"),
                       label = "Task",
                       choices = c("-", taskGroup_tasks()$name),
                       selected = NULL,
                       multiple = FALSE,
                       width = "200px")
        
      } else {NULL}
      
    })
    
    
    # -- Observe: filter
    observeEvent(input$filter_task, {
      
      if(input$filter_task != "-"){
        
        # get id
        id <- taskGroup_tasks()[taskGroup_tasks()$name == input$filter_task, ]$id
        
        # trace
        cat("[TASK] Filter selection: id =", as.character(id), "name =", input$filter_task, "\n")
        
      } else {id <- NULL}
      
      # cache id
      r$filter_Task(id)
      
    }, ignoreInit = TRUE)
    
    
    # -------------------------------------
    # [ACTION BUTTONS]
    # -------------------------------------
    
    # -- UI: Inputs
    output$action_buttons <- renderUI(tagList(
      
      # Button: always ON
      actionButton(ns("btn_new"), "New"),
      
      # Button: when selected task
      if(!is.null(selectedTask())){
        
        # get task
        task <- r$tasks()[r$tasks()$id == selectedTask(), ]
        
        tagList(
          
          # Button: delete
          actionButton(ns("btn_delete"), "Delete"),
          
          # Button: obsolete
          if(task$status != "obsolete"){
            actionButton(ns("btn_obsolete"), "Obsolete")},
          
          # Button: set TD
          if(is.na(task$date.planned)){
            tagList(
              dateInput(ns("val_targetDate"), "", value = Sys.Date(), weekstart = 1, width = 200),
              actionButton(ns("btn_targetDate"), "Set TD"))},
          
          # Button: Progress-
          if((task$status == "In Progress" && task$progress != 0) || task$status == "Done") {
            actionButton(ns("btn_progressMinus"), "Progress -")},
          
          # Button: Progress+
          if((task$status == "Not Started" || task$status == "In Progress") && task$progress < 100){
            actionButton(ns("btn_progressPlus"), "Progress +")},
          
          # Button: Done
          if(task$status != "draft" && task$status != "Done"){
            tagList(
              dateInput(ns("val_dateDone"), "", value = task$date.planned, weekstart = 1, width = 200),
              actionButton(ns("btn_done"), "Done"))})}))
    
    
    # -- Observe: click new button
    observeEvent(input$btn_new, {
      
      # display form
      showModal(modalDialog(
        
        title = "Create task",
        textInput(ns("task_name"), "Name",
                  placeholder = 'Choose name'),
        textInput(ns("task_note"), "Note",
                  placeholder = 'Type something'),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("task_create"), "Create"))))
      
      
    })
    
    
    # -- Observe: confirm new (modal)
    observeEvent(input$task_create, {
      
      # required
      req(input$task_name)
      
      # close form
      removeModal()
      
      # check filter_TaskGroup
      if(!is.null(r$filter_TaskGroup())){
        
        # new project
        new_item <- data.frame(id = ktools::getTimestamp(),
                               name = input$task_name,
                               date.create = as.POSIXct(Sys.time()),
                               date.planned = NA,
                               date.done = NA,
                               deplayed = 0,
                               project.id = r$active_project(),
                               task.group.id = r$filter_TaskGroup(),
                               before = "xxx",
                               after = "xxx",
                               progress = 0,
                               status = "draft",
                               milestone.id = 0,
                               note = input$task_note)
        
        
        
        # merge
        new_item_list <- rbind(r$tasks(), new_item)
        
        # cache & save
        storeAndSave(new_item_list)
        
      }else{
        showNotification("Error: select a task group before submit!", type = "error")
      }
      
    })
    
    
    # -- Button: Delete
    observeEvent(input$btn_delete, {
      
      # log
      cat("[TASK] Delete selected item \n")
      
      # drop item 
      new_item_list <- r$tasks()[r$tasks()$id != selectedTask(), ]
      
      # cache & save
      storeAndSave(new_item_list)
      
    })
    
    
    # -- Button: Obsolete
    observeEvent(input$btn_obsolete, {
      
      # log
      cat("[TASK] Set selected item as obsolete \n")
      
      # get task list
      tmp_tasks <- r$tasks()
      
      # remove TD
      tmp_tasks[tmp_tasks$id == selectedTask(), ]$date.planned <- NA
      # remove progress
      tmp_tasks[tmp_tasks$id == selectedTask(), ]$progress <- 0
      # change status
      tmp_tasks[tmp_tasks$id == selectedTask(), ]$status <- "obsolete"
      
      # cache & save
      storeAndSave(tmp_tasks)
      
    })
    
    
    # -- Button: Set TD 
    observeEvent(input$btn_targetDate, {
      
      # value required
      req(input$val_targetDate)
      
      cat("[TASK] Set target date \n")
      
      # get task list
      tmp_tasks <- r$tasks()
      
      # set TD
      tmp_tasks[tmp_tasks$id == selectedTask(), ]$date.planned <- as.POSIXct(as.Date(input$val_targetDate, "1970-01-01"))
      # change status
      tmp_tasks[tmp_tasks$id == selectedTask(), ]$status <- "Not Started"
      
      # cache & save
      storeAndSave(tmp_tasks)
      
    })
    
    # -- Button: Progress + 
    observeEvent(input$btn_progressPlus, {
      
      cat("[TASK] Set progress + \n")
      
      # get task list
      tmp_tasks <- r$tasks()
      
      # change status
      if(tmp_tasks[tmp_tasks$id == selectedTask(), ]$status == "Not Started"){
        tmp_tasks[tmp_tasks$id == selectedTask(), ]$status <- "In Progress"
      }
      # get progress and update
      progress <- tmp_tasks[tmp_tasks$id == selectedTask(), ]$progress
      if(progress < 100){
        tmp_tasks[tmp_tasks$id == selectedTask(), ]$progress <- progress + 5}
      
      # cache & save
      storeAndSave(tmp_tasks)
      
    })
    
    # -- Button: Progress -
    observeEvent(input$btn_progressMinus, {
      
      cat("[TASK] Set progress - \n")
      
      # get task list
      tmp_tasks <- r$tasks()
      
      # get progress and update
      progress <- tmp_tasks[tmp_tasks$id == selectedTask(), ]$progress
      if(progress > 0){
        tmp_tasks[tmp_tasks$id == selectedTask(), ]$progress <- progress - 5}
      
      # change status
      if(tmp_tasks[tmp_tasks$id == selectedTask(), ]$progress == 0){
        tmp_tasks[tmp_tasks$id == selectedTask(), ]$status <- "Not Started"}
      
      if(progress == 100){
        tmp_tasks[tmp_tasks$id == selectedTask(), ]$status <- "In Progress"
        tmp_tasks[tmp_tasks$id == selectedTask(), ]$date.done <- ""}
      
      # cache & save
      storeAndSave(tmp_tasks)
      
    })
    
    # -- Button: Done
    observeEvent(input$btn_done, {
      
      req(input$val_dateDone)
      
      cat("[TASK] Set selected task as done \n")
      
      # get task list
      tmp_tasks <- r$tasks()
      
      # set date done
      tmp_tasks[tmp_tasks$id == selectedTask(), ]$date.done <- as.POSIXct(as.Date(input$val_dateDone, "1970-01-01"))
      # set progress
      tmp_tasks[tmp_tasks$id == selectedTask(), ]$progress <- 100
      # change status
      tmp_tasks[tmp_tasks$id == selectedTask(), ]$status <- "Done"
      
      # cache & save
      storeAndSave(tmp_tasks)
      
    })
    
    
    # -------------------------------------
    # [TODO TAB]
    # -------------------------------------
    
    # -- UI: Table
    output$todo.list <- renderDT(filtered_tasks()[filtered_tasks()$status %in% c("Not Started", "In Progress"), ][-c(1, 7)],
                                 rownames = FALSE,
                                 options = list(lengthMenu = c(10, 20, 30), pageLength = 10, dom = "ltpf"),
                                 selection = list(mode = 'single', target = "row"))
    
    
    # -------------------------------------
    # [KPIs]
    # -------------------------------------
    
    # -- KPI: progress
    output$kpi_progress <- renderUI(expr = {
      
      # compute value
      progress <- round(mean(filtered_tasks()$progress), digits = 0)
      valueBox(paste(progress, "%"), "Progress", color = "light-blue", width = 12)})
    
    
    # -- KPI: overdue
    output$kpi_overdue <- renderUI(expr = {
      
      # compute value
      overdue <- dim(filtered_tasks()[filtered_tasks()$status == "In Progress" & filtered_tasks()$date.planned < as.POSIXct(Sys.time()), ])[1]
      valueBox(overdue, "Overdue", color = ifelse(overdue > 0, "red", "green"), width = 12)})
    
    
    # -- KPI: next TD
    output$kpi_nextTD <- renderUI(expr = {
      
      # compute value
      next_td <- as.Date(min(filtered_tasks()$date.planned, na.rm = TRUE))
      valueBox(next_td, "Next TD", color = "light-blue", width = 12)})
    
    
    # -- KPI: task chart
    output$kpi_taskStatus <- renderPlot({
      
      # hack to avoid blank plot
      p <- getTaskStatusPlot(filtered_tasks())
      # return
      p
      
    }, width = "auto", height = 200, bg = "transparent")
    
    
    # -- KPI: on time delivery
    output$kpi_onTimeStatus <- renderPlot({
      
      # hack to avoid blank plot
      p <- getOnTimeDeliveryPlot(filtered_tasks())
      # return
      p
      
    }, width = "auto", height = 200, bg = "transparent")
    
    
    # -- KPI: task history
    output$kpi_taskHistory <- renderPlot({
      
      # hack to avoid blank plot
      p <- getTaskHistoryPlot(filtered_tasks())
      # return
      p
      
    }, width = "auto", height = 300, bg = "transparent")
    
  })
}

