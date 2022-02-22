

# -- Workload
getWorkloadPlot <- function(timesheet){
  
  # formate date (remove time)
  timesheet$date <- as.Date(timesheet$date)
  
  # group by date
  timesheet <- timesheet %>%
    group_by(date) %>%
    summarise(workload = sum(time))
  
  
  # build plot
  ggplot(timesheet, aes(x = date, y = workload)) +
    geom_point() +
    geom_area(color = "black", fill = "#E69F00", alpha = 0.5, linetype = "dashed") +
    
    # apply theme
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
}
