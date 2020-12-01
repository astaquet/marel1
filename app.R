#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
# 01-kmeans-app



ui <- fluidPage(
    headerPanel('data2007final k-means clustering'),
    sidebarPanel(
        selectInput('xcol', 'X Variable', names(data2007final)),
        selectInput('ycol', 'Y Variable', names(data2007final),
                    selected = names(data2007final)[[2]]),
        
    ),
    mainPanel(
        plotOutput('plot1')
    )
)

server <- function(input, output) {
    
    selectedData <- reactive({
        data2007final[, c(input$xcol, input$ycol)]
    })
    
    
    
    output$plot1 <- renderPlot({
        par(mar = c(5.1, 4.1, 0, 1))
        plot(selectedData(),
             col = "blue", type='l',
             pch = 20, cex = 3)
        
    })
    
}

shinyApp(ui = ui, server = server)


# Run the application 
shinyApp(ui = ui, server = server)


