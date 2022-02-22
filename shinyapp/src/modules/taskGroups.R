
# --------------------------------------------------------------------------------
# Shiny module: taskGroups
# --------------------------------------------------------------------------------

# -- Library


# -- Source
source ("~/Work/R/Library/Time and date/getTimestamp.R")


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


# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

taskGroupManager_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {

    # get namespace
    ns <- session$ns
    
    # -- declare
    filename <- "task_groups.csv"
    cols <- c(id = "double",
              name = "character",
              project.id = "double",
              before = "character",
              after = "character")
    
    # -- declare communication connectors
    r$filter_TaskGroup  <- reactiveVal(NULL)
    
    # -- declare internal objects
    selectedTaskGroup <- reactiveVal(NULL)
    
    # -- declare internal views
    project_taskgroups <- NULL
    filtered_taskgroups <- NULL
    
    # -------------------------------------
    # [DATA & VIEWS]
    # -------------------------------------
    
    # -- load data
    r$taskGroups <- reactiveVal(read.data(path$data, filename, cols))
    
    
    # -- store & save
    storeAndSave <- function(new_item_list){
      
      # cache
      r$taskGroups(new_item_list)
      
      # save
      write.data(r$taskGroups(), path$data, filename)
      
      # log & notify
      cat("[TASKGROUP] Item list saved \n")
      showNotification("TaskGroup saved.", type = "message")
      
    }

    
    # -- project view
    project_taskgroups <- reactive({

      # check
      if(!is.null(r$active_project())){
        
        # apply filter
        cat("[TASKGROUP] Apply active project \n")
        r$taskGroups()[r$taskGroups()$project.id == r$active_project(), ]
        
      } else {r$taskGroups()}
      
    })
    
    
    # -- filtered view
    filtered_taskgroups <- reactive({
      
      # log
      cat("[TASKGROUP] Apply taskgroup filters \n")
      
      # check filter
      if(!is.null(r$filter_TaskGroup())){
        
        project_taskgroups()[project_taskgroups()$id == r$filter_TaskGroup(), ]
        
      } else {project_taskgroups()}
      
    })
      

    # -------------------------------------
    # [TABLE]
    # -------------------------------------
    
    # -- Item table
    output$taskGroupTable <- renderDT(filtered_taskgroups()[-c(1, 3)],
                                      options = list(lengthMenu = c(5, 10, 15), pageLength = 5, dom = "ltp"),
                                      rownames = FALSE,
                                      selection = 'single')
    
    
    # -- In table item selection
    observeEvent(input$taskGroupTable_rows_selected, ignoreNULL = FALSE, {

      # check input
      if(!is.null(input$taskGroupTable_rows_selected) & length(input$taskGroupTable_rows_selected) != 0){
        
        # get id and name
        id <- filtered_taskgroups()[input$taskGroupTable_rows_selected, ]$id
        name <- filtered_taskgroups()[input$taskGroupTable_rows_selected, ]$name
        
        # trace
        cat("[TASKGROUP] In table selection: id =", as.character(id), "name =", name, "\n")
        
      } else {id <- NULL}
      
      # cache id
      selectedTaskGroup(id)
      
    })
    
    
    # -------------------------------------
    # [FILTER]
    # -------------------------------------
    
    # -- Define: inputs
    output$taskgroup_filter <- renderUI(
      
      # Selector
      selectizeInput(ns("taskgroupFilter"),
                     label = "TaskGroup",
                     choices = c("-", project_taskgroups()$name),
                     selected = NULL,
                     multiple = FALSE,
                     width = '200px'))
    
    
    # -- Observe: taskgroupFilter
    observeEvent(input$taskgroupFilter, {
      
      if(input$taskgroupFilter != "-"){
        
        # get id
        id <- project_taskgroups()[project_taskgroups()$name == input$taskgroupFilter, ]$id
        
        # trace
        cat("[TASKGROUP] Filter selection: id =", as.character(id), "name =", input$taskgroupFilter, "\n")
        
      } else {id <- NULL}
      
      # cache id
      r$filter_TaskGroup(id)
      
    }, ignoreInit = TRUE)
    
    
    # -------------------------------------
    # [ACTION BUTTONS]
    # -------------------------------------
    
    # -- Define: action buttons
    output$action_buttons <- renderUI(tagList(

          # Button: always ON
          actionButton(ns("btn_new"), "Create"),
          
          # Button: when task group selected 
          if(!is.null(selectedTaskGroup())){
            
            actionButton(ns("btn_delete"), "Delete")}))
    
    
    # -- Observe: new
    observeEvent(input$btn_new, {
      
      # display modal
      showModal(modalDialog(
        
        title = "Create task group",
        textInput(ns("taskgroup_name"), "Name",
                  placeholder = 'Choose name'),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("taskgroup_create"), "Create"))))
      
    })
    
    
    # -- Observe: confirm new
    observeEvent(input$taskgroup_create, {

      # required
      req(input$taskgroup_name)

      # close form
      removeModal()
      
      # check selected project
      if(!is.null(r$active_project())){
        
        # new project
        new_item <- data.frame(id = getTimestamp(),
                               name = input$taskgroup_name,
                               project.id = r$active_project(),
                               before = "xxx",
                               after = "xxx")
        
        # merge
        new_item_list <- rbind(r$taskGroups(), new_item)
        
        # store & save
        storeAndSave(new_item_list)
        
      }else{
        showNotification("Error: select a project before submit!", type = "error")
      }
    
    })
    
    
    # -- Observe: delete
    observeEvent(input$btn_delete, {
      
      cat("[TASKGROUP] Delete taskGroup \n")
      
      # remove TD
      tmp_taskGroups <- r$taskGroups()[r$taskGroups()$id != selectedTaskGroup(), ]
      
      # store & save
      storeAndSave(new_item_list)
      
    })
    
    
  })
}



