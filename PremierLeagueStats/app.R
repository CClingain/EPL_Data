#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Premier League Stats Comparison"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
           selectizeInput(inputId = "x",
                     label = "Team 1",
                     choices = unique(EPL_aggregate$Team),
                     selected = "Liverpool"),
      selectizeInput(inputId = "a",
                     label = "Team 2",
                     choices = unique(EPL_aggregate$Team),
                     selected = "Arsenal"),
      
      selectizeInput(inputId = "y",
                     label = "Season",
                     choices = unique(EPL_aggregate$Season)),
      selectizeInput(inputId = "z",
                     label = "Statistic",
                     choices = c("Shots Taken" = "Shots_Taken_Total","Shots Conceded" = "Shots_Conceded_Total",
                                 "Shots on Target" = "Shots_on_Target_Total","Shots on Target Conceded" = "Shots_on_Target_Conceded_Total",
                                 "Fouls Committed" = "Fouls_Committed_Total", "Fouls Against" = "Fouls_Against_Total",
                                 "Corners" = "Corners_Total", "Corners Conceded" = "Corners_Conceded_Total",
                                 "Yellow Cards" = "Yellow_Cards_Total", "Red Cards" = "Red_Cards_Total") ,
                     selected = "Shots Taken")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required
server <- function(input, output) {
  #data
  #read csv
    EPL_aggregate <- read.csv("EPL_aggregate.csv")
    Team <- unique(EPL_aggregate$Team)
  output$distPlot <- renderPlot({
    # select team and season
    
    data1 <- EPL_aggregate %>% filter(Team == input$x) %>% filter(Season == input$y)
    data2 <- EPL_aggregate %>% filter(Team == input$a) %>% filter(Season == input$y)
    var <- gsub("_", " ", input$z, fixed=TRUE)
    Team1 <- data1$Team[1]
    Team2 <- data2$Team[1]
  # title <-  paste(data1$Team[1], "vs", data2$Team[1], "on", var)
   #   title <-  paste(Team1, "vs", Team2) 
    yaxis <- paste(gsub("_", " ", input$z, fixed=TRUE))
    names <- c(data1$Team[1],data2$Team[1])
    
    # draw the plot
    
  plot(data1$Week, as.vector(as.matrix(data1[colnames(data1)==input$z])), type ="l", col = 2, xlab = "Week", ylab = ylab, ylim = c(0,90), xlim = c(0,38), main = "", cex.lab = 1.5)
    lines(data2$Week, as.vector(as.matrix(data2[colnames(data2)==input$z])), col = 4)
   # title(main= title)
    legend("topleft",legend=names,pch=15, fill=c(2,4), cex=1)
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

