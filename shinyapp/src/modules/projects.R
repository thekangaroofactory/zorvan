
# --------------------------------------------------------------------------------
# Shiny module: projects
# --------------------------------------------------------------------------------

# -- Library


# -- Source
source ("~/Work/R/Library/Time and date/getTimestamp.R")


# -------------------------------------
# [TABLE]
# -------------------------------------

# -- Project table UI
# projectTable_UI <- function(id)
# {
# 
#   # namespace
#   ns <- NS(id)
# 
#   # table
#   DTOutput(ns("projectTable"),
#            width = 600)
# 
# }


# -------------------------------------
# [SET ACTIVE PROJECT]
# -------------------------------------

# -- Select Project
projectSelector_Input <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("project_selector"))

}


# -------------------------------------
# [ACTION BUTTONS]
# -------------------------------------

# -- Action buttons
projectActions_BTN <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # UI
  uiOutput(ns("action_buttons"))
  
}


# ------------------------------------------------------------------------------
# Server logic
# ------------------------------------------------------------------------------

projectManager_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {

    # get namespace
    ns <- session$ns
    
    # -- declare
    filename <- "projects.csv"
    cols <- c(id = "double",
             name = "character",
             category = "character")
    
    # -- Init selected item
    r$active_project <- reactiveVal(NULL)
    
    
    # -------------------------------------
    # Load data
    # -------------------------------------
    
    # -- load data
    r$projects <- reactiveVal(read.data(path$data, filename, cols))
    
    
    # -------------------------------------
    # [TABLE]
    # -------------------------------------
    
    # -- Project table
    # output$projectTable <- renderDT(r$projects()[-c(1)],
    #                                 options = list(lengthMenu = c(5, 10, 15),
    #                                                pageLength = 5,
    #                                                dom = "tp"),
    #                                 #editable = list(target = 'row'),
    #                                 rownames = FALSE,
    #                                 selection = 'single')
    
    
    # -- In table item selection
    # observeEvent(input$projectTable_rows_selected, ignoreNULL = FALSE, {
    # 
    #   # get id and name
    #   id <- r$projects()[input$projectTable_rows_selected, ]$id
    #   name <- r$projects()[input$projectTable_rows_selected, ]$name
    # 
    #   #trace
    #   cat("Selected project: id =", as.character(id), "name =", name, "\n")
    # 
    #   #cache id
    #   r$xxxxx(id) << changer !!!
    # 
    # })
    
    
    # -------------------------------------
    # [SET ACTIVE PROJECT]
    # -------------------------------------
    
    # -- Input:
    output$project_selector <- renderUI(
      
      # Selector
      selectizeInput(ns("select_project"),
                     label = "Select active project",
                     choices = r$projects()$name,
                     selected = NULL,
                     multiple = FALSE,
                     width = '200px'))

    
    # -- Observe:
    observeEvent(input$select_project, {
      
      # get id
      id <- r$projects()[r$projects()$name == input$select_project, ]$id
      
      # trace
      cat("[PROJECT] Filter selection: id =", as.character(id), "name =", input$select_project, "\n")
      
      # cache id
      r$active_project(id)
      
    })
    
    
    # -------------------------------------
    # [ACTION BUTTONS]
    # -------------------------------------
    
    # -- Buttons:
    output$action_buttons <- renderUI(tagList(
      
      # Buttons: always on
      actionButton(ns("btn_new"), "New"),
      actionButton(ns("btn_delete"), "Delete")))
    
    
    # -- Observe: new
    observeEvent(input$btn_new, {
      
      showModal(modalDialog(
        
        title = "Create project",
        
        textInput(ns("project_name"), "Name",
                  placeholder = 'Choose project name'),
        textInput(ns("project_category"), "Category",
                  placeholder = 'Choose project category'),
        
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("project_create"), "Create"))))
      
    })
    
    
    # -- Observe: create project
    observeEvent(input$project_create, {
      
      # required
      req(input$project_name,
          input$project_category)
      
      # close modal
      removeModal()
      
      # new project
      new_item <- data.frame(id = getTimestamp(),
                             name = input$project_name,
                             category = input$project_category)
      
      # merge
      r$projects(rbind(r$projects(), new_item))
      
      # save
      write.data(r$projects(), path$data, filename)
      
      # notify
      showNotification("Project created.", type = "message")

    })
    
    # -- Observe: delete
    observeEvent(input$btn_delete, {
      
      cat("[BTN] Delete project \n")
      # TODO: cat("Compute impacts for project id =", r$active_project, "\n")
      # getImpacts(...)
      
      # -- check when no project exists
      if(length(r$active_project()) == 0){
        
        # notify
        showNotification("There is no project to delete", type = "warning")
        
      } else {
        
        showModal(modalDialog(
          
          p("Do you confirm project deletion?"),
          
          title = "Delete project",
          
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("project_delete"), "Confirm delete"))))}
        
    })
    
    
    # -- Observe: Confirm Delete
    observeEvent(input$project_delete, {
    
      # close window
      removeModal()
      
      # get project list
      project_list <- r$projects()
      
      # drop project
      project_list <- project_list[!project_list$id == r$active_project(), ]
      
      # store
      r$projects(project_list)
      
      # save
      write.data(r$projects(), path$data, filename)
      
      # notify
      showNotification("Project deleted", type = "message")})
    

  })
}

