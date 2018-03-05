#####################################
### Data Cleaning & Manipulation
#####################################

#NOTES:
# *This R script is for combining the datasets on the English Premier League results from 2000/2001
#  to 2017/2018. Data for the current season will be updated weekly.
#  *For information regarding variable names/labels, please see codebook.txt
#  *All data sets were pulled from http://www.football-data.co.uk/englandm.php*

#  *This project is just for fun! Combining my love of statistics/data and football!
#  *Goal is to create a Shiny App where football lovers can interact with EPL stats.
####################################################################################################

##Loading all datasets and preparing merges

#/2017-2018 (will be updated each week)
s17.18 <- read.csv("2017-2018.csv")

#The best package!! Will be using throughout
library(dplyr)

#Not interested in the betting odds, so going to remove those; also don't need division (all EPL)
names(s17.18)
s17.18 <- s17.18[,2:23]
s17.18$Date <- as.Date(s17.18$Date, format = "%d/%m/%y")

#/2016-2017
s16.17 <- read.csv("2016-2017.csv")
names(s16.17) #always check the names! order can change
s16.17 <- s16.17[,2:23]
s16.17$Date <- as.Date(s16.17$Date, format = "%d/%m/%y")

#/2015-2016
s15.16 <- read.csv("2015-2016.csv")
names(s15.16)
s15.16 <- s15.16[,2:23]
s15.16$Date <- as.Date(s15.16$Date, format = "%d/%m/%y")


#/2014-2015
s14.15 <- read.csv("2014-2015.csv")
names(s14.15)
s14.15 <- s14.15[,2:23]
#remove last empty row
s14.15 <- s14.15[-381,]
s14.15$Date <- as.Date(s14.15$Date, format = "%d/%m/%y")


#/2013-2014
s13.14 <- read.csv("2013-2014.csv")
names(s13.14)
s13.14 <- s13.14[,2:23]
s13.14$Date <- as.Date(s13.14$Date, format = "%d/%m/%y")


#/2012-2013
s12.13 <- read.csv("2012-2013.csv")
names(s12.13)
s12.13 <- s12.13[,2:23]
s12.13$Date <- as.Date(s12.13$Date, format = "%d/%m/%y")


#/2011-2012
s11.12 <- read.csv("2011-2012.csv")
names(s11.12)
s11.12 <- s11.12[,2:23]
s11.12$Date <- as.Date(s11.12$Date, format = "%d/%m/%y")

#/2010-2011
s10.11 <- read.csv("2010-2011.csv")
names(s10.11)
s10.11 <- s10.11[,2:23]
s10.11$Date <- as.Date(s10.11$Date, format = "%d/%m/%y")

#/2009-2010
s09.10 <- read.csv("2009-2010.csv")
names(s09.10)
s09.10 <- s09.10[,2:23]
s09.10$Date <- as.Date(s09.10$Date, format = "%d/%m/%y")

#/2008-2009
s08.09 <- read.csv("2008-2009.csv")
names(s08.09)
s08.09 <- s08.09[,2:23]
s08.09$Date <- as.Date(s08.09$Date, format = "%d/%m/%y")

#/2007-2008
s07.08 <- read.csv("2007-2008.csv")
names(s07.08)
s07.08 <- s07.08[,2:23]
s07.08$Date <- as.Date(s07.08$Date, format = "%d/%m/%y")

#/2006-2007
s06.07 <- read.csv("2006-2007.csv")
names(s06.07)
s06.07 <- s06.07[,2:23]
s06.07$Date <- as.Date(s06.07$Date, format = "%d/%m/%y")

#/2005-2006
s05.06 <- read.csv("2005-2006.csv")
names(s05.06)
s05.06 <- s05.06[,2:23]
s05.06$Date <- as.Date(s05.06$Date, format = "%d/%m/%y")

#/2004-2005
s04.05 <- read.csv("2004-2005.csv")
names(s04.05)
s04.05 <- s04.05[,2:23]
s04.05 <- na.omit(s04.05) #there are random empty rows, get rid of em!
s04.05$Date <- as.Date(s04.05$Date, format = "%d/%m/%y")

#/2003-2004
s03.04 <- read.csv("2003-2004.csv")
names(s03.04)
s03.04 <- s03.04[,2:23]
s03.04 <- na.omit(s03.04) #there are random empty rows, get rid of em!
s03.04$Date <- as.Date(s03.04$Date, format = "%d/%m/%y")

#/2002-2003
s02.03 <- read.csv("2002-2003.csv")
names(s02.03)
s02.03 <- s02.03[,2:23]
s02.03 <- na.omit(s02.03)
#/02-03 has a different date format from all the rest!
s02.03$Date <- as.Date(s02.03$Date, format = "%d/%m/%Y")

#/2001-2002
s01.02 <- read.csv("2001-2002.csv")
names(s01.02)
s01.02 <- s01.02[,2:30] #Select out all football stats
s01.02 <- s01.02[,-c(10,16,17,22,23,28,29)] #now remove the ones not found in others
s01.02$Date <- as.Date(s01.02$Date, format = "%d/%m/%y")

#/2000-2001
s00.01 <- read.csv("2000-2001.csv")
names(s00.01)
s00.01 <- s00.01[,2:30] #Select out all football stats 
s00.01 <- s00.01[,-c(10,16,17,22,23,28,29)] #remove ones not found in others
s00.01$Date <- as.Date(s00.01$Date, format = "%d/%m/%y")

#NOTE: order of variables is slightly different in s01.02 and s00.01 compared to rest
#Reverse order: HST AST HC AC HF AF
#Compared to rest: HST AST HF AF HC AC
s00.01 <- s00.01[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,18,15,16,19,20,21,22)]
s01.02 <- s01.02[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,17,18,15,16,19,20,21,22)]

#All ready!
##################################################################################################

#Joining the datasets to create master file: EPL
#  *Note: will not be specifying keys since there aren't any! (i.e., changes every season)

EPL <- full_join(s17.18,s16.17)
EPL <- full_join(EPL, s15.16)
EPL <- full_join(EPL, s14.15)
EPL <- full_join(EPL, s13.14)
EPL <- full_join(EPL, s12.13)
EPL <- full_join(EPL, s11.12)
EPL <- full_join(EPL, s10.11)
EPL <- full_join(EPL, s09.10)
EPL <- full_join(EPL, s08.09)
EPL <- full_join(EPL, s07.08)
EPL <- full_join(EPL, s06.07)
EPL <- full_join(EPL, s05.06)
EPL <- full_join(EPL, s04.05)
EPL <- full_join(EPL, s03.04)
EPL <- full_join(EPL, s02.03)
EPL <- full_join(EPL, s01.02)
EPL <- full_join(EPL, s00.01)

#that's a lot of matches!

##################################################################################################

#Data manipulation: adding + mutating columns

#Making a season column
EPL <- EPL %>% mutate(season = Date)
EPL$season <- as.character(EPL$season)
#to change this, I can using indexing since I know the order in which I merged the data sets
EPL[1:280,23] <- "17/18"
EPL[281:660, 23] <- "16/17"
EPL[661:1040,23] <- "15/16"
EPL[1041:1420,23] <- "14/15"
EPL[1421:1800,23] <- "13/14"
EPL[1801:2180,23] <- "12/13"
EPL[2181:2560,23] <- "11/12"
EPL[2561:2940,23] <- "10/11"
EPL[2941:3320,23] <- "09/10"
EPL[3321:3700,23] <- "08/09"
EPL[3701:4080,23] <- "07/08"
EPL[4081:4460,23] <- "06/07"
EPL[4461:4840,23] <- "05/06"
EPL[4841:5220,23] <- "04/05"
EPL[5221:5600,23] <- "03/04"
EPL[5601:5980,23] <- "02/03"
EPL[5981:6360,23] <- "01/02"
EPL[6361:6740,23] <- "00/01"

#Making a calendar year column (this will be handy later on)
#mutate year column
EPL <- EPL %>% mutate(year = Date)
#now to save only the years
EPL$year <- format(EPL$year,"%Y")

#check
unique(EPL$year)
#Now let's put the matches in chronological order
EPL <- EPL[order(EPL$Date, decreasing=T),]

#save dataset
library(readr)
write_excel_csv(EPL, "~/EPL Data/Clean Data/EPL.csv")

##################################################################################################
#League Standings 17/18 Season

Teams <- c("Arsenal","Bournemouth","Brighton and Hove Albion","Burnley","Chelsea","Crystal Palace",
           "Everton","Huddersfield Town","Leicester City","Liverpool","Manchester City","Manchester United",
           "Newcastle","Southampton","Stoke City","Swansea","Tottenham Hotspur","Watford","West Bromwich Albion","West Ham United")
Week29 <- c(45,33,34,40,53,27,34,30,37,60,78,59,29,28,27,30,58,36,20,30)
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
PointsTable <- data.frame(Teams, Week29, Week28, Week27, Week26, Week25, 
                       Week24, Week23,Week22, Week21, Week20, Week19,
                       Week18, Week17, Week16, Week15, Week14, Week13,
                       Week12, Week11, Week10, Week09, Week08, Week07,
                        Week06, Week05, Week04, Week03, Week02, Week01, stringsAsFactors = F)

write_excel_csv(PointsTable, "~/EPL Data/Clean Data/PointsTable17-18.csv")

