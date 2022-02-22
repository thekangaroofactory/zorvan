
# --------------------------------------------------------------------------------
# Shiny module: workout
# --------------------------------------------------------------------------------

# -- Library


# -- Source
source("~/Work/R/Library/Read and write/read.data.R")
source("~/Work/R/Library/Read and write/write.data.R")


# UI items logic
# --------------

workoutUI <- function(id) {
  
  # namespace
  ns <- NS(id)
  
  # taglist
  tagList(
    tableOutput(ns("workouts"))
  )
  
}


# Input items logic
# -----------------

sessionInput <- function(id) {
  
  # namespace
  ns <- NS(id)
  
  # taglist
  wellPanel(
    dateInput(ns("date"), "Date", value = as.Date(Sys.Date()), weekstart = 1),
    numericInput(ns("duration"), "Duration", value = 1),
    textInput(ns("activity"), "Activity", value = "Enter text..."),
    radioButtons(ns("type"), "Type",
                 choices = list("Muscu" = 1,
                                "Cardio" = 2),
                 selected = 1),
    numericInput(ns("distance"), "Distance", value = 0),
    actionButton(ns("submit"), "Submit")
  )
  
}


# Server logic
# ------------

workoutServer <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {

    # -- declare
    filename <- "workouts.csv"
    cols <- c(date = "character",
              duration = "numeric",
              activity = "character",
              type = "integer",
              distance = "numeric")
    
    # -- load data
    workouts <- reactiveVal(read.data(path$data, filename, cols))
    
    # -- ouputs
    output$workouts <- renderTable(workouts())
    
    # -- New input event observer
    observeEvent(input$submit, {
      
      # required
      req(input$date,
          input$activity,
          input$type)
      
      # new session
      new_workout <- data.frame(date = as.character(as.Date(input$date, "1970/01/01")) ,
                            duration = input$duration,
                            activity = input$activity,
                            type = input$type,
                            distance = input$distance)
      
      # merge
      workouts(rbind(workouts(), new_workout))
      
      #save
      write.data(workouts(), path$data, filename)
      
      
    })
    
  })
}


# helper functions
# ----------------

