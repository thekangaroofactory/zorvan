
# -- dependencies
library(ggplot2)


# [How it works]
# Donut or pie plot does not exist in ggplot2 which is intended since pie charts are considered bad practice...
# The donut is build this way:
# - build a rectangle chart
# - apply coord_polar transformation to make it a pie
# - apply xlim to make the donut hole
  

#' Donut Chart
#'
#' @param data a data.frame with these columns: category, count (if more they will be ignored)
#' @param color a vector defining the colors to be applied. If color has length = 1 (single value), the
#' ColorBrewer palette = color will be used. If length > 1, the vector is used as a color palette.
#'
#' @return a plot (from ggplot2)
#' @export
#'
#' @examples
#' 
#' # Create test data.
#' mydata <- data.frame(
#'   category=c("Draft", "Planned", "InWork", "Done"),
#'   count=c(10, 20, 30, 40))
#'   
#' # Create color palette
#' mycolor <- c("#999999", "#E69F00", "#56B4E9", "#999999")
#' 
#' # donut with given colors
#' donut(mydata, mycolor)
#' 
#' # donut with ColorBrewer palette
#' donut(mydata, 5)
#' 
#' # donut with ColorBrewer palette
#' donut(mydata, "PRGn")
#' 
#' # donut with default ColorBrewer palette (PiYG)
#' donut(mydata)
#' 


donut <- function(data, color = NULL){
  
  # log
  #cat("Building donut chart \n")
  
  # check color
  if(is.null(color))
    color <- "PiYG"
  
  
  # Compute percentages
  data$fraction = data$count / sum(data$count)
  
  # Compute the cumulative percentages (top of each rectangle)
  data$ymax = cumsum(data$fraction)
  
  # Compute the bottom of each rectangle
  data$ymin = c(0, head(data$ymax, n = -1))
  
  # Compute label position
  data$labelPosition <- (data$ymax + data$ymin) / 2
  
  # Compute a good label
  data$label <- paste0(data$category, "\n", data$count)

  # Make basic plot
  p <- ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = category)) +
    
    # -- build the base rectangle
    geom_rect() +
    
    # -- add text label
    geom_text(x = 5, aes(y = labelPosition, label = label, color = category), size = 3) # x here controls label position (inner / outer)
  
  # Apply color based on argument
  if(length(color) == 1){
    
    p <- p + scale_fill_brewer(palette = color) +
      scale_color_brewer(palette = color)
    
  } else {
    
    p <- p + scale_fill_manual(values = color) +
      scale_color_manual(values = color)
    
  } 
  
  # -- apply transformation 
  p <- p  + coord_polar(theta = "y") + # Try to remove that to understand how the chart is built initially
    xlim(c(-1, 4)) + # Try to remove that to see how to make a pie chart
    
    # -- theme
    theme_void() +
    theme(legend.position = "none")
      
}

