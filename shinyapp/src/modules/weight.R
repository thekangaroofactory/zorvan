
# --------------------------------------------------------------------------------
# Shiny module: xxxx
# --------------------------------------------------------------------------------

# -- Library
#library(xxxx)

# -- Source dependancies
# source(xxxx)


# -------------------------------------
# UI items section
# -------------------------------------

# -- Table
weightTable_UI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # table
  tableOutput(ns("itemTable"))
  
}


# -------------------------------------
# Input items section
# -------------------------------------

# -- New item input form
weightNew_Input <- function(id) {
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    
    # inputs
    dateInput(ns("date"), "Date", value = Sys.Date(), weekstart = 1),
    
    numericInput(ns("weight"), "Weight", value = 0, min = 0, step = 0.1),
    numericInput(ns("fat.rate"), "Fat rate", value = 0, min = 0, max = 100, step = 0.1),
    numericInput(ns("muscle.rate"), "Muscle rate", value = 0, min = 0, max = 100, step = 0.1),
    numericInput(ns("muscle.weight"), "Muscle weight", value = 0, min = 0, step = 0.1),
    numericInput(ns("chest"), "Chest", value = 0, min = 0, step = 0.1),
    numericInput(ns("waist"), "Waist", value = 0, min = 0, step = 0.1),
    numericInput(ns("biceps"), "Biceps", value = 0, min = 0, step = 0.1),
    
    # submit
    actionButton(ns("submit_new"), "Submit")
  )
  
}


# -------------------------------------
# Server logic
# -------------------------------------

weight_Server <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {

    # -- declare
    filename <- "weight.csv"
    cols <- c(date = "character",
              weight = "numeric",
              fat.rate = "numeric",
              muscle.rate = "numeric",
              muscle.weight = "numeric",
              chest = "numeric",
              waist = "numeric",
              biceps = "numeric")
    
    # -- load data
    r$weight <- reactiveVal(read.data(path$data, filename, cols))
    
    
    # -------------------------------------
    # Outputs
    # -------------------------------------
    
    # -- Item table
    output$itemTable <- renderTable(r$weight())
    
    
    # -------------------------------------
    # Event observers
    # -------------------------------------
    
    # -- New item submit button
    observeEvent(input$submit_new, {
      
      # required
      req(input$date,
          input$weight,
          input$fat.rate,
          input$muscle.rate,
          input$muscle.weight)
      
      # new project
      new_item <- data.frame(date = as.character(as.Date(input$date, "1970/01/01")),
                             weight = input$weight,
                             fat.rate = input$fat.rate,
                             muscle.rate = input$muscle.rate,
                             muscle.weight = input$muscle.weight,
                             chest = input$chest,
                             waist = input$waist,
                             biceps = input$biceps)
      
      # merge
      r$weight(rbind(r$weight(), new_item))
      
      #save
      write.data(r$weight(), path$data, filename)
      
    })
    
  })
}


# -------------------------------------
# helper functions section
# -------------------------------------

