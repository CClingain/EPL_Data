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

#/2016-2017
s16.17 <- read.csv("2016-2017.csv")
names(s16.17) #always check the names! order can change
s16.17 <- s16.17[,2:23]

#/2015-2016
s15.16 <- read.csv("2015-2016.csv")
names(s15.16)
s15.16 <- s15.16[,2:23]

#/2014-2015
s14.15 <- read.csv("2014-2015.csv")
names(s14.15)
s14.15 <- s14.15[,2:23]
#remove last empty row
s14.15 <- s14.15[-381,]

#/2013-2014
s13.14 <- read.csv("2013-2014.csv")
names(s13.14)
s13.14 <- s13.14[,2:23]

#/2012-2013
s12.13 <- read.csv("2012-2013.csv")
names(s12.13)
s12.13 <- s12.13[,2:23]

#/2011-2012
s11.12 <- read.csv("2011-2012.csv")
names(s11.12)
s11.12 <- s11.12[,2:23]

#/2010-2011
s10.11 <- read.csv("2010-2011.csv")
names(s10.11)
s10.11 <- s10.11[,2:23]

#/2009-2010
s09.10 <- read.csv("2009-2010.csv")
names(s09.10)
s09.10 <- s09.10[,2:23]

#/2008-2009
s08.09 <- read.csv("2008-2009.csv")
names(s08.09)
s08.09 <- s08.09[,2:23]

#/2007-2008
s07.08 <- read.csv("2007-2008.csv")
names(s07.08)
s07.08 <- s07.08[,2:23]

#/2006-2007
s06.07 <- read.csv("2006-2007.csv")
names(s06.07)
s06.07 <- s06.07[,2:23]

#/2005-2006
s05.06 <- read.csv("2005-2006.csv")
names(s05.06)
s05.06 <- s05.06[,2:23]

#/2004-2005
s04.05 <- read.csv("2004-2005.csv")
names(s04.05)
s04.05 <- s04.05[,2:23]
s04.05 <- na.omit(s04.05) #there are random empty rows, get rid of em!

#/2003-2004
s03.04 <- read.csv("2003-2004.csv")
names(s03.04)
s03.04 <- s03.04[,2:23]
s03.04 <- na.omit(s03.04) #there are random empty rows, get rid of em!

#/2002-2003
s02.03 <- read.csv("2002-2003.csv")
names(s02.03)
s02.03 <- s02.03[,2:23]
s02.03 <- na.omit(s02.03)

#/2001-2002
s01.02 <- read.csv("2001-2002.csv")
names(s01.02)
s01.02 <- s01.02[,2:30] #Select out all football stats
s01.02 <- s01.02[,-c(10,16,17,22,23,28,29)] #now remove the ones not found in others

#/2000-2001
s00.01 <- read.csv("2000-2001.csv")
names(s00.01)
s00.01 <- s00.01[,2:30] #Select out all football stats 
s00.01 <- s00.01[,-c(10,16,17,22,23,28,29)] #remove ones not found in others

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
#The date column we curretly have imported as a character -- let's change that

EPL$Date <- as.Date(EPL$Date,format = "%d/%m/%y")
#mutate year column
EPL <- EPL %>% mutate(year = Date)
#now to save only the years
EPL$year <- format(EPL$year,"%Y")