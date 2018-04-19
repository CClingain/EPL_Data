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
                     label = "Statistic*",
                     choices = c("Shots Taken" = "Shots_Taken_Total","Shots Conceded" = "Shots_Conceded_Total",
                                 "Shots on Target" = "Shots_on_Target_Total","Shots on Target Conceded" = "Shots_on_Target_Conceded_Total",
                                 "Fouls Committed" = "Fouls_Committed_Total", "Fouls Against" = "Fouls_Against_Total",
                                 "Corners" = "Corners_Total", "Corners Conceded" = "Corners_Conceded_Total",
                                 "Yellow Cards" = "Yellow_Cards_Total", "Red Cards" = "Red_Cards_Total") ,
                     selected = "Shots Taken"),
      p("*All statistics are aggregated over the season."),
      p("NOTE: A team that was not in the Premier League for a specific season will appear as NA in the legend.")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("newPlot")
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
    title <-  paste(data1$Team[1], "vs", data2$Team[1], "on", var)
    #   title <-  paste(Team1, "vs", Team2) 
    ylab <- paste(gsub("_", " ", input$z, fixed=TRUE))
    names <- c(data1$Team[1],data2$Team[1])
    
    # draw the plot
    
    par(mar=c(3, 4, 3, 2), oma=c(0,0,0,0), bg="#F0F0F0", xpd=FALSE, xaxs="r", yaxs="i", mgp=c(2.1,.3,0), las=1, col.axis="#434343", col.main="#343434", tck=0, lend=1)
    
    plot(data1$Week, as.vector(as.matrix(data1[colnames(data1)==input$z])), type ="l", col = 2, xlab = "Week", ylab = ylab, ylim = c(0,max(as.vector(as.matrix(data1[colnames(data1)==input$z])),as.vector(as.matrix(data2[colnames(data2)==input$z])))), xlim = c(0,38), main = "", cex.lab = 1.5,lwd=2)
    lines(data2$Week, as.vector(as.matrix(data2[colnames(data2)==input$z])), col = 4,lwd=2)
    title(main= title)
    legend("topleft",legend=c(paste(Team1), paste(Team2)), fill=c(2,4), cex=1)
  })
  
  output$newPlot <- renderPlot({
    # select team and season
    
    data1 <- EPL_aggregate %>% filter(Team == input$x) %>% filter(Season == input$y)
    data2 <- EPL_aggregate %>% filter(Team == input$a) %>% filter(Season == input$y)
    var <- gsub("_", " ", input$z, fixed=TRUE)
    Team1 <- data1$Team[1]
    Team2 <- data2$Team[1]
    title <-  paste("Week by Week:",data1$Team[1], "vs", data2$Team[1], "on", gsub("Total","",var))
    #   title <-  paste(Team1, "vs", Team2) 
    ylab <- paste(gsub("_", " ", input$z, fixed=TRUE))
    names <- c(data1$Team[1],data2$Team[1])
    #prepare the data
    name1 <- gsub("_Total","", input$z)
    count <- cbind(as.vector(as.matrix(data1[colnames(data1)==as.character(name1)])),
                   as.vector(as.matrix(data2[colnames(data2)==as.character(name1)])))
    # draw the plot
    
    par(mar=c(3, 4, 3, 2), oma=c(0,0,0,0), bg="#F0F0F0", xpd=FALSE, xaxs="r", yaxs="i", mgp=c(2.1,.3,0), las=1, col.axis="#434343", col.main="#343434", tck=0, lend=1)
    barplot(count, col = c(2,4), xlab = "Week", ylab = ylab,space=c(0,.2), ylim = c(0,max(as.vector(as.matrix(data1[colnames(data1)==as.character(name1)])),as.vector(as.matrix(data2[colnames(data2)==as.character(name1)])))), xlim = c(1,76), main = "", cex.lab = 1.5, beside = T, names.arg = rep(1:38,each=2))
    title(main= title)
    legend("topleft",legend=c(paste(Team1), paste(Team2)), fill=c(2,4), cex=1)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

