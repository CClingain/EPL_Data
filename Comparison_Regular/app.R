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
   titlePanel("Comparing Teams 2017-2018"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "x",
                       label = "Team 1",
                       choices = c( "Arsenal" = 1, "Bournemouth" = 2,"Brighton and Hove Albion" =3,"Burnley" =4,"Chelsea"=5,"Crystal Palace"=6,
                                      "Everton"=7,"Huddersfield Town"=8,"Leicester City"=9,"Liverpool"=10,"Manchester City"=11,"Manchester United"=12,
                                      "Newcastle"=13,"Southampton"=14,"Stoke City"=15,"Swansea"=16,"Tottenham Hotspur"=17,"Watford"=18,"West Bromwich Albion"=19,"West Ham United"=20),
                       selected = 1),
        
        selectizeInput(inputId = "y",
                       label = "Team 2",
                       choices = c("Arsenal" = 1, "Bournemouth" = 2,"Brighton and Hove Albion" =3,"Burnley" =4,"Chelsea"=5,"Crystal Palace"=6,
                                      "Everton"=7,"Huddersfield Town"=8,"Leicester City"=9,"Liverpool"=10,"Manchester City"=11,"Manchester United"=12,
                                      "Newcastle"=13,"Southampton"=14,"Stoke City"=15,"Swansea"=16,"Tottenham Hotspur"=17,"Watford"=18,"West Bromwich Albion"=19,"West Ham United"=20),
                       selected = 2)
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
  Teams <- c("Arsenal","Bournemouth","Brighton and Hove Albion","Burnley","Chelsea","Crystal Palace",
             "Everton","Huddersfield Town","Leicester City","Liverpool","Manchester City","Manchester United",
             "Newcastle","Southampton","Stoke City","Swansea","Tottenham Hotspur","Watford","West Bromwich Albion","West Ham United")
  Week30 <- c(48,33,34,43,56,27,37,31,40,60,81,65,32,28,27,31,61,36,20,30)
  Week29 <- c(45,33,34,40,53,27,34,30,37,60,78,62,29,28,27,30,58,36,20,30)
  Week28 <- c(45,32,31,37,53,27,34,30,36,57,75,59,29,27,26,27,55,33,20,30)
  Week27 <- c(45,31,28,36,53,27,34,27,35,54,72,56,28,26,25,27,52,30,20,30)
  Week26 <- c(45,31,27,36,50,27,31,24,35,51,69,56,25,26,24,24,49,30,20,27)
  Week25 <- c(42,28,24,35,50,26,31,24,34,50,68,53,24,23,24,23,48,27,20,27)
  Week24 <- c(42,25,23,34,50,25,28,24,34,47,65,53,23,22,23,20,45,26,20,26)
  Week23 <- c(39,24,23,34,47,25,27,24,31,47,62,50,23,21,20,17,44,26,19,25)
  Week22 <- c(38,21,23,34,46,22,27,24,30,44,62,47,22,20,20,16,41,25,16,22)
  Week21 <- c(38,20,22,34,45,19,27,24,27,41,59,44,19,20,20,16,37,25,16,18)
  Week20 <- c(37,17,21,33,42,18,27,23,27,38,58,43,18,19,20,13,37,25,15,18)
  Week19 <- c(34,16,21,32,39,18,26,22,27,35,55,42,18,19,19,13,34,22,14,17)
  Week18 <- c(33,16,18,32,38,17,25,21,26,34,52,41,15,18,16,12,31,22,14,17)
  Week17 <- c(30,16,17,31,35,14,22,18,26,31,49,38,15,18,16,12,31,22,14,14)
  Week16 <- c(29,16,17,28,32,11,19,18,23,30,46,35,15,18,16,12,28,22,13,13)
  Week15 <- c(28,15,17,25,32,10,18,15,20,29,43,35,15,17,16,9,25,22,13,10)
  Week14 <- c(28,14,17,25,29,9,15,15,17,26,40,32,15,16,13,9,24,21,12,10)
  Week13 <- c(25,14,16,22,26,8,12,15,14,23,37,29,14,16,13,9,24,21,11,10)
  Week12 <- c(22,13,16,22,25,5,12,15,13,22,34,26,14,13,13,8,23,18,10,9)
  Week11 <- c(19,10,15,19,22,4,11,15,13,19,31,23,14,13,12,8,23,15,10,9)
  Week10 <- c(19,7,12,16,19,4,8,12,12,16,28,23,14,13,11,8,20,15,10,9)
  Week09 <- c(16,7,11,13,16,3,8,12,9,13,25,20,14,12,8,8,20,15,10,8)
  Week08 <- c(13,4,8,13,13,3,8,9,6,13,22,20,11,9,8,8,17,15,10,8)
  Week07 <- c(13,4,7,12,13,0,7,9,5,12,19,19,10,8,8,5,14,12,9,7)
  Week06 <- c(10,3,7,9,13,0,7,9,4,11,16,16,9,8,5,5,11,11,8,4)
  Week05 <- c(7,3,4,8,10,0,4,8,4,8,13,13,9,8,5,5,8,8,8,4)
  Week04 <- c(6,0,4,7,9,0,4,7,3,7,10,10,6,5,5,4,7,8,7,3)
  Week03 <- c(3,0,1,4,6,0,4,7,3,7,9,7,3,5,4,4,4,5,7,0)
  Week02 <- c(3,0,0,3,3,0,4,6,3,4,6,4,0,4,3,1,3,4,6,0)
  Week01 <- c(3,0,0,3,0,0,3,3,0,1,3,3,0,1,0,1,3,1,3,0)
  
  #Putting it all together into a data frame
  PointsTable <- data.frame(Teams, Week30,Week29, Week28, Week27, Week26, Week25, 
                            Week24, Week23,Week22, Week21, Week20, Week19,
                            Week18, Week17, Week16, Week15, Week14, Week13,
                            Week12, Week11, Week10, Week09, Week08, Week07,
                            Week06, Week05, Week04, Week03, Week02, Week01, stringsAsFactors = F)
  PointsTable.long<- reshape(PointsTable[,2:31], varying=c("Week01","Week02", "Week03", "Week04", "Week05", "Week06", 
                                                           "Week07", "Week08","Week09", "Week10", "Week11", "Week12",
                                                           "Week13", "Week14", "Week15", "Week16", "Week17", "Week18",
                                                           "Week19", "Week20", "Week21", "Week22", "Week23", "Week24",
                                                           "Week25", "Week26", "Week27", "Week28", "Week29", "Week30"), 
                             v.names='Week',timevar="Time",idvar="Teams",direction="long")
  Teams <- c("Arsenal","Bournemouth","Brighton and Hove Albion","Burnley","Chelsea","Crystal Palace",
             "Everton","Huddersfield Town","Leicester City","Liverpool","Manchester City","Manchester United",
             "Newcastle","Southampton","Stoke City","Swansea","Tottenham Hotspur","Watford","West Bromwich Albion","West Ham United")
  
  
   output$distPlot <- renderPlot({
      # generate teams
      
      data1 <- PointsTable.long %>% filter(Teams == input$x)
      data2 <- PointsTable.long %>% filter(Teams == input$y)
      title <-  paste(Teams[data1$Teams[1]], "vs", Teams[data2$Teams[1]]) 
      names <- c(Teams[data1$Teams[1]],Teams[data2$Teams[1]])
      # draw the plot
      plot(data1$Time, data1$Week, type ="l", col = 2, xlab = "Week", ylab = "Points", ylim = c(0,90), xlim = c(0,30), main = title, cex.main = 2, cex.lab = 1.5)
      lines(data2$Time, data2$Week, col = 4)
      legend("topleft", names,pch=15, col= c(2,4), cex =1)
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

