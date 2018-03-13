
library(shiny)
library(plotly)
# Define UI for application 
ui <- fluidPage(
  plotlyOutput("plot"),
  
   # Application title
   titlePanel("Comparing Two Premier League Teams 17/18"),
   
   # Sidebar with input for choosing teams
   sidebarLayout(
      sidebarPanel(
         selectizeInput(inputId = "T1",
                     label = "Team 1",
                     choices = unique(Teams1),
                     selected = "Arsenal"),
         
         selectizeInput(inputId = "T2",
                     label = "Team 2",
                     choices = unique(Teams2),
                     selected = "Bournemouth")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotlyOutput("plot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
   #data
  T1 <- c("Arsenal","Bournemouth","Brighton and Hove Albion","Burnley","Chelsea","Crystal Palace",
          "Everton","Huddersfield Town","Leicester City","Liverpool","Manchester City","Manchester United",
          "Newcastle","Southampton","Stoke City","Swansea","Tottenham Hotspur","Watford","West Bromwich Albion","West Ham United")
  T2 <- c("Arsenal","Bournemouth","Brighton and Hove Albion","Burnley","Chelsea","Crystal Palace",
          "Everton","Huddersfield Town","Leicester City","Liverpool","Manchester City","Manchester United",
          "Newcastle","Southampton","Stoke City","Swansea","Tottenham Hotspur","Watford","West Bromwich Albion","West Ham United")
  
  PointsTable <- read.csv("~/EPL Data/Clean Data/PointsTable17-18.csv")
  #reshaping the Points Table for 2017-2018 for graphing purposes
  PointsTable.long<- reshape(PointsTable[,2:31], varying=c("Week01","Week02", "Week03", "Week04", "Week05", "Week06", "Week07", "Week08","Week09", "Week10", "Week11", "Week12","Week13", "Week14", "Week15", "Week16", "Week17", "Week18","Week19", "Week20", "Week21", "Week22", "Week23", "Week24",
                                                           "Week25", "Week26", "Week27", "Week28", "Week29", "Week30"), v.names='Week',timevar="Time",idvar="Teams",direction="long")
  
   output$plot <- renderPlotly({
     
     plot_ly(PointsTable.long, x = ~Time[Teams1==input$T1], y = ~Week[Teams1==input$T1], name = input$T1, type = 'scatter', mode = 'lines') %>%
       add_trace(y = ~Week[Teams2==input$T2], name = input$T2, mode = 'lines') %>%
       layout(title = "Point Accumulation for 2017-2018 season", xaxis = list(title = "Week"), yaxis = list(title = "# of points"))
     
   })
  
}


# Run the application 
shinyApp(ui = ui, server = server)

