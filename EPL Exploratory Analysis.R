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
#only match with 9 Away YCs (highest of any match) was the one that decided
#that Leiceister won the league!

#Home Yellow Cards
table(EPL$HY)

#Away Red Cards
table(EPL$AR)

#Home Red Cards
table(EPL$HR)


