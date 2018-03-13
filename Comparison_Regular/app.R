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

# Define server logic required to draw a histogram
server <- function(input, output) {
   #data
  PointsTable <- read.csv("~/EPL Data/Clean Data/PointsTable17-18.csv")
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
      # generate bins based on input$bins from ui.R
      
      data1 <- PointsTable.long %>% filter(Teams == input$x)
      data2 <- PointsTable.long %>% filter(Teams == input$y)
      title <-  paste(Teams[data1$Teams[1]], "vs", Teams[data2$Teams[1]]) 
      names <- c(Teams[data1$Teams[1]],Teams[data2$Teams[1]])
      # draw the histogram with the specified number of bins
      plot(data1$Time, data1$Week, type ="l", col = 2, xlab = "Week", ylab = "Points", ylim = c(0,90), xlim = c(0,30), main = title, cex.main = 2, cex.lab = 1.5)
      lines(data2$Time, data2$Week, col = 4)
      legend("topleft", names,pch=15, col= c(2,4), cex =1)
     })
}

# Run the application 
shinyApp(ui = ui, server = server)

