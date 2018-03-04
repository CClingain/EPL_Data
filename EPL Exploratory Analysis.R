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
#only match with 9 Away YCs (highest of any match) was the one that decided that Leiceister won the league!

#Home Yellow Cards
table(EPL$HY)

#Visualizing the two Yellow Card distributions
plot(density(EPL$AY), main = "Yellow Cards per Match 2000-2018", xlab = "# of Yellow Cards in a Match")
lines(density(EPL$HY), col = "red")

#the pretty viz!
plot(density(EPL$HY),main = "Yellow Cards per Match 2000-2018", xlab = "# of Red Cards", lty = "solid", bty = "l", xlim=c(0,10))
polygon(density(EPL$HY), col = rgb(red = 139, green = 0, blue = 0, alpha = 100, maxColorValue = 255), lty = "solid")
polygon(density(EPL$AY), col = rgb(red = 0, green = 0, blue = 130, alpha = 100, maxColorValue = 255), lty = "dotted")
legend(x = 4, y = 0.6, legend = c("Home Yellow Cards","Away Yellow Cards"),
       lty = "solid" , col = c("red","blue"), cex=.7)

mean(EPL$AY)
mean(EPL$HY)
median(EPL$AY)
median(EPL$HY)

#Since we don't have normality
wilcox.test(EPL$AY, EPL$HY)


#Away Red Cards
table(EPL$AR)

#Home Red Cards
table(EPL$HR)