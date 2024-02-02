
# --------------------------------------------------------------------------------
# This is the user-interface definition of the Shiny web application
# --------------------------------------------------------------------------------

# -- Library

library(shiny)
library(shinydashboard)
library(DT)


# -- load Modules & Scripts
cat("[UI] Source code... \n")

code_path <- "../shinyapp/src"
for (nm in list.files(code_path, full.names = TRUE, recursive = TRUE, include.dirs = FALSE))
{
  cat("Loading from: ", nm, "\n")
  source(nm)
}
rm(nm)


# -- Define UI

# Header
header <- dashboardHeader(title = "Zorvân")


# Sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "dashboard", icon = icon("project-diagram")),
    menuItem("Roadmap", tabName = "roadmap", icon = icon("project-diagram")),
    menuItem("Projects", tabName = "project", icon = icon("project-diagram"), selected = TRUE)))


# Body
body <- dashboardBody(
  
  tabItems(
    
    # -- DASHBOARD
    tabItem(tabName = "dashboard",
            
            h4("Global follow-up, effort")),
    
    # -- DASHBOARD
    tabItem(tabName = "roadmap",
            
            h4("Global roadmap idk")),
    
    # -- PROJECTS
    tabItem(tabName = "project",
            
            wellPanel(fluidRow(
              
              column(width = 8,
                     div(projectSelector_Input("project"), style = "display:inline-block;"),
                     div(taskgroupFilter_Input("taskGroup"), style = "display:inline-block;"),
                     div(taskFilter_INPUT("task"), style = "display:inline-block;")),
              
              column(width = 4,
                     h4("Project actions"),
                     projectActions_BTN("project")))),
            
            fluidRow(
              tabBox(
                title = "Project details",
                id = "tabset2",
                width = 12,
                
                # -- Tab:
                tabPanel("Follow up",
                         
                         # -- Metrics
                         fluidRow(
                           
                           # -- 1st column
                           column(width = 2,
                                  
                                  fluidRow(
                                    column(width = 12, kpiProgress_UI("task"))),
                                  
                                  fluidRow(
                                    column(width = 12, kpiEffort_UI("time"))),
                                  
                                  fluidRow(
                                    column(width = 12, kpiOverdue_UI("task"))),
                                  
                                  fluidRow(
                                    column(width = 12, kpiLastEffort_UI("time"))),
                                  
                                  fluidRow(
                                    column(width = 12, kpiNextTD_UI("task")))),
                           
                           
                           # -- 2nd column
                           column(width = 5,
                                  
                                  fluidRow(
                                    column(width = 12, wellPanel(p("Task status"),
                                                                 kpiTaskStatus_PLOT("task")))),
                                  
                                  fluidRow(
                                    column(width = 12, wellPanel(p("Task history"),
                                                                 kpiTaskHistory_PLOT("task"))))),
                           
                           
                           # -- 3rd column
                           column(width = 5,
                                  
                                  fluidRow(
                                    column(width = 12, wellPanel(p("On time delivery"),
                                                                 kpiOnTimeStatus_PLOT("task")))),
                                  
                                  fluidRow(
                                    column(width = 12, wellPanel(p("Workload"),
                                                                 kpiWorkload_PLOT("time"))))))),
                
                
                # -- Tab:
                tabPanel("Tasks",
                         
                         fluidRow(
                           
                           # -- task group 
                           column(width = 4,
                                  wellPanel(
                                    h4("TaskGroup"),
                                    taskGroupsTable_UI("taskGroup"),
                                    taskGroup_BTN("taskGroup"))),
                           
                           # -- task
                           column(width = 8,
                                  wellPanel(
                                    h4("Task"),
                                    taskTable_UI("task"),
                                    taskButtons_UI("task"))))),
                
                
                # -- Tab:
                tabPanel("Time",
                         wellPanel(
                           h4("Timesheet"),
                           timesheetTable_project_UI("time"),
                           timeSheet_buttons_UI("time"))),
                
                
                
                # -- Tab: ToDo
                tabPanel("ToDo list",
                         taskTodoList_UI("task"))
                
                
              )) # tabBox
    ) # tabItem
    
  ) # tabItems
) # dashboardBody


# -- Put them together into a dashboard

dashboardPage(
  header,
  sidebar,
  body
)

