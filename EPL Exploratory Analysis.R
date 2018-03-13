########################################################
### Data Tables & Statistics: Exploratory Analysis
########################################################

#NOTES:
# *The main purpose of this document is to get a sense of the data

######################################################################################################
# -Disciplinary Stats-

#Away Yellow Cards
table(EPL$AY) 
which(EPL$AY == 9)
EPL[685,]
#only match with 9 Away YCs (highest of any match) was the one that decided that Leicester won the league!

#Home Yellow Cards
table(EPL$HY)

#Visualizing the two Yellow Card distributions
plot(density(EPL$AY), main = "Yellow Cards per Match 2000-2018", xlab = "# of Yellow Cards in a Match")
lines(density(EPL$HY), col = "red")

#the pretty viz!
plot(density(EPL$HY),main = "Yellow Cards per Match 2000-2018", xlab = "# of Yellow Cards", lty = "solid", bty = "l", xlim=c(0,10))
polygon(density(EPL$HY), col = rgb(red = 139, green = 0, blue = 0, alpha = 100, maxColorValue = 255), lty = "solid")
polygon(density(EPL$AY), col = rgb(red = 0, green = 0, blue = 130, alpha = 100, maxColorValue = 255), lty = "dotted")
legend(x = 4, y = 0.6, legend = c("Home Yellow Cards","Away Yellow Cards"),
       lty = "solid" , col = c("red","blue"), cex=.7)

mean(EPL$AY)
mean(EPL$HY)
median(EPL$AY)
median(EPL$HY)

#Since we don't have normality
wilcox.test(EPL$AY, EPL$HY, alternative = "two.sided")
#however, large enough sample size, could find sig diference...

#Away Red Cards
table(EPL$AR)

#Home Red Cards
table(EPL$HR)

###################################################################################################
#reshaping the Points Table for 2017-2018 for graphing purposes
PointsTable.long<- reshape(PointsTable[,2:31], varying=c("Week01","Week02", "Week03", "Week04", "Week05", "Week06", 
                                                       "Week07", "Week08","Week09", "Week10", "Week11", "Week12",
                                                       "Week13", "Week14", "Week15", "Week16", "Week17", "Week18",
                                                       "Week19", "Week20", "Week21", "Week22", "Week23", "Week24",
                                                      "Week25", "Week26", "Week27", "Week28", "Week29", "Week30"), 
                                                      v.names='Week',timevar="Time",idvar="Teams",direction="long")

#basic plots of point accumulation for each team so far
for(i in 1:20){
  plot(PointsTable.long$Time[PointsTable.long$Teams==i], PointsTable.long$Week[PointsTable.long$Teams==i], type = "l", col = 2, xlab = "Week", ylab = "Points", main = Teams[i])
}

#separate tables for each team
Arsenal <- PointsTable.long %>% filter(Teams == 1)
Bournemouth <- PointsTable.long %>% filter(Teams == 2)
Brighton <- PointsTable.long %>% filter(Teams == 3)
Burnley <- PointsTable.long %>% filter(Teams == 4)
Chelsea <- PointsTable.long %>% filter(Teams == 5)
CrystalPalace <- PointsTable.long %>% filter(Teams == 6)
Everton <- PointsTable.long %>% filter(Teams == 7)
Huddersfield <- PointsTable.long %>% filter(Teams == 8)
Leicester <- PointsTable.long %>% filter(Teams == 9)
Liverpool <- PointsTable.long %>% filter(Teams == 10)
ManCity <- PointsTable.long %>% filter(Teams == 11)
ManUnited <- PointsTable.long %>% filter(Teams == 12)
Newcastle <- PointsTable.long %>% filter(Teams == 13)
Southampton <- PointsTable.long %>% filter(Teams == 14)
Stoke <- PointsTable.long %>% filter(Teams == 15)
Swansea <- PointsTable.long %>% filter(Teams == 16)
Tottenham <- PointsTable.long %>% filter(Teams == 17)
Watford <- PointsTable.long %>% filter(Teams == 18)
WestBrom <- PointsTable.long %>% filter(Teams == 19)
WestHam <- PointsTable.long %>% filter(Teams == 20)

#Now we can plot comparing two or more teams in a clearer manner (will need to adjust ylim)
plot(Arsenal$Time, Arsenal$Week, type ="l", col = 2, xlab = "Week", ylab = "Points", ylim = c(0,90))
lines(Bournemouth$Time, Bournemouth$Week, col = 3)
lines(ManCity$Time, ManCity$Week, col = 4)


#Now to upgrade the plots! We don't need the restructured data set for this either -- very helpful
library(plotly)
#Comparing all 20 teams
a <- plot_ly(PointsTable.long, x = ~Time[Teams==1], y = ~Week[Teams==1], name = 'Arsenal', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~Week[Teams==2], name = 'Bournemouth', mode = 'lines') %>%
  add_trace(y = ~Week[Teams==3], name = "Brighton", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==4], name = "Burnley", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==5], name = "Chelsea", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==6], name = "Crystal Palace", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==7], name = "Everton", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==8], name = "Huddersfield", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==9], name = "Leicester", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==10], name = "Liverpool", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==11], name = "Manchester City", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==12], name = "Manchester United", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==13], name = "Newcastle", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==14], name = "Southamptom", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==15], name = "Stoke", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==16], name = "Swansea", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==17], name = "Tottenham", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==18], name = "Watford", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==19], name = "West Brom", mode = 'lines') %>%
  add_trace(y = ~Week[Teams==20], name = "West Ham", mode = 'lines') %>%
  layout(title = "Point Accumulation for 2017-2018 season", xaxis = list(title = "Week"), yaxis = list(title = "# of points"))
a

##################################################################################################
