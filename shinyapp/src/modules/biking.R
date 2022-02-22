
# --------------------------------------------------------------------------------
# Shiny module: biking
# --------------------------------------------------------------------------------

# -- Library
library(ggplot2)

# -- Source
source("~/Work/R/Library/Read and write/read.data.R")
source("~/Work/R/Library/Read and write/write.data.R")


# -------------------------------------
# UI items section
# -------------------------------------

# -- Biking weekly table
bikingTableUI <- function(id)
{
  
  # namespace
  ns <- NS(id)
  
  # table
  tableOutput(ns("table"))
  
}


# -- Biking weekly plot
bikingPlotUI <- function(id) {
  
  # namespace
  ns <- NS(id)
  
  plotOutput(ns("plot"))
  
}


# -- Biking target valuebox
bikingTargetUI <- function(id) {
  
  # namespace
  ns <- NS(id)
  
  valueBoxOutput(ns("target"))
  
}


# -- Biking current valuebox
bikingCurrentUI <- function(id) {
  
  # namespace
  ns <- NS(id)
  
  valueBoxOutput(ns("current"))
  
}


# -- Biking progress valuebox
bikingProgressUI <- function(id) {
  
  # namespace
  ns <- NS(id)
  
  valueBoxOutput(ns("progress"))
  
}


# -------------------------------------
# Input items section
# -------------------------------------

# -- Biking input form
bikingInput <- function(id) {
  
  # namespace
  ns <- NS(id)
  
  # UI
  wellPanel(
    dateInput(ns("date"), "Date", value = as.Date(Sys.Date()), weekstart = 1),
    numericInput(ns("distance"), "Distance", value = 0),
    actionButton(ns("submit"), "Submit")
  )
  
}


# -------------------------------------
# Server logic
# -------------------------------------

bikingServer <- function(id, r, path) {
  moduleServer(id, function(input, output, session) {

    # -- declare
    filename <- "biking.csv"
    cols <- c(date = "character", distance = "numeric", progress = "numeric")
    
    # -- load data
    biking <- reactiveVal(read.data(path$data, filename, cols))
    
    
    # -- outputs
    
    output$table <- renderTable(tail(biking(), n = 5))
    
    output$plot <- renderPlot(getBikePlot(biking()))
    
    output$target <- renderValueBox(
      valueBox(paste(1500, "km"), "Target", icon = icon("biking")))
    
    output$current <- renderValueBox(
      valueBox(paste(max(biking()$distance), "km"), "Current", icon = icon("biking")))
    
    output$progress <- renderValueBox(
      valueBox(paste(round(max(biking()$distance)/1500*100, digits = 0), "%"), "Progress", icon = icon("biking")))
    
    
    # -- New input event observer
    observeEvent(input$submit, {
      
      # required
      req(input$date,
          input$distance)
      
      cat("New biking input \n")
      
      # get inputs 
      date <- as.character(as.Date(input$date, "1970/01/01")) 
      distance <- input$distance
      
      # get highest distance
      previous <- max(biking()$distance)

      
      # compute progress
      progress <- distance - previous
      
      
      # new row
      new_row <- data.frame(date = date,
                            distance = distance,
                            progress = progress)
      
      # merge
      biking(rbind(biking(), new_row))
      
      #save
      write.data(biking(), path$data, filename)
      
      
    })
    
  })
}


# -------------------------------------
# helper functions section
# -------------------------------------

# -- plot data
getBikePlot <- function(biking)
{
  
  cat("Generate biking graph \n")
  
  ggplot(data = biking, aes(x = as.Date(date), y = distance)) +
    
    # plot total distance
    geom_point(size = 2) +
    geom_line() +
    geom_area(fill = "#E69F00", alpha = 0.3) +
    
    # plot progress
    geom_bar(aes(y = progress), stat = 'identity', alpha = 0.3) +
    
    # remove axis labels
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    
    # add lines
    geom_hline(yintercept = 1500)
  
}
