Aggregating Data
================

### Data Structuring

The purpose of this file is to structure the data in a format which makes aggregate and time series analysis and visualization easier. We start with the Cleaned Data `EPL.csv` for this.

``` r
EPL <- read.csv("Clean Data/EPL.csv")
head(EPL)
```

    ##         Date       HomeTeam    AwayTeam FTHG FTAG FTR HTHG HTAG HTR
    ## 1 2018-03-01        Arsenal    Man City    0    3   A    0    3   A
    ## 2 2018-02-25 Crystal Palace   Tottenham    0    1   A    0    0   D
    ## 3 2018-02-25     Man United     Chelsea    2    1   H    1    1   D
    ## 4 2018-02-24    Bournemouth   Newcastle    2    2   D    0    2   A
    ## 5 2018-02-24       Brighton     Swansea    4    1   H    1    0   H
    ## 6 2018-02-24        Burnley Southampton    1    1   D    0    0   D
    ##      Referee HS AS HST AST HF AF HC AC HY AY HR AR season year
    ## 1 A Marriner 10  9   5   5 11 11  6  1  1  1  0  0  17/18 2018
    ## 2   K Friend  5 14   3   4 12  5  2 13  0  1  0  0  17/18 2018
    ## 3 M Atkinson 10 14   5   7 13 12  3  4  2  2  0  0  17/18 2018
    ## 4     R East 18 15   5   4 12 10  8  1  1  4  0  0  17/18 2018
    ## 5     M Dean 16 11   5   1 17  6  3  5  1  0  0  0  17/18 2018
    ## 6   R Madley  9 13   3   6  8 14  7  5  1  0  0  0  17/18 2018

The structure of the reformatted table will be as follows: - Season - Year - Week - Team - Opponent - Home\_Away - Goals\_Scored - Goals\_Conceded - Points\_Earned\_Week - Points\_Total - Shots\_Taken - Shots\_Conceded - Shots\_Taken\_Total - Shots\_Conceded\_Total - Shots\_on\_Target - Shots\_on\_Target\_Conceded - Shots\_on\_Target\_Total - Shots\_on\_Target\_Conceded\_Total - Fouls\_Committed - Fouls\_Against - Fouls\_Committed\_Total - Fouls\_Against\_Total - Corners - Corners\_Total - Corners\_Conceded - Corners\_Conceded\_Total - Yellow\_Cards - Yellow\_Cards\_Total - Red\_Cards - Red\_Cards\_Total

This table will be sorted by Season, Week and Team in that order.

``` r
EPL.aggregate <- data.frame(matrix(NA,nrow=1,ncol=30))
unique_seasons <- sort(unique(EPL$season))

## Even though, it's a triple loop, we only look at subset of data at each stage hence still O(2*N)
## 2*N because we look at each match twice. 

for(i in seq_along(unique_seasons)){ ## looping through each season
  tmp <- EPL[EPL$season == unique_seasons[i],]
  tmp <- tmp[order(tmp$Date),] ## ensuring sort by date
  unique_teams <- sort(unique(tmp$HomeTeam)) ## looping through teams for the season
  
  season.df <- data.frame(matrix(NA,nrow=1,ncol=30))
  
  for(j in seq_along(unique_teams)){  
    games <- grepl(unique_teams[j],as.character(tmp$HomeTeam)) |
      grepl(unique_teams[j],as.character(tmp$AwayTeam))
    games <- tmp[games,]
    
    team.df <- data.frame(matrix(0,nrow=(dim(games)[1]+1),ncol=30))
    team.df[,1] <- rep(unique_seasons[i],(dim(games)[1]+1))
    team.df[,3] <- c(0:dim(games)[1])
    team.df[,4] <- rep(unique_teams[j],(dim(games)[1]+1))
    
    for(k in c(1:dim(games)[1])){ ## looping through games in season
      team.df[(k+1),2] <- games$year[k]
      h_a <- grep(unique_teams[j],as.matrix(games[k,2:3]))
      team.df[(k+1),5] <- ifelse(h_a == 1,as.character(games[k,3]),as.character(games[k,2]))
      team.df[(k+1),6] <- ifelse(h_a == 1,"H","A")
      team.df[(k+1),7] <- ifelse(h_a == 1,games[k,4],games[k,5])
      team.df[(k+1),8] <- ifelse(h_a == 1,games[k,5],games[k,4])
      team.df[(k+1),9] <- (team.df[(k+1),7] > team.df[(k+1),8])*2 + 1 - (team.df[(k+1),7] < team.df[(k+1),8])*1
      team.df[(k+1),10] <- team.df[(k),9] + team.df[(k+1),9]
      team.df[(k+1),11] <- ifelse(h_a == 1,games[k,11],games[k,12])
      team.df[(k+1),12] <- ifelse(h_a == 1,games[k,12],games[k,11])
      team.df[(k+1),13] <- team.df[(k),11] + team.df[(k+1),11]
      team.df[(k+1),14] <- team.df[(k),12] + team.df[(k+1),12]
      team.df[(k+1),15] <- ifelse(h_a == 1,games[k,13],games[k,14])
      team.df[(k+1),16] <- ifelse(h_a == 1,games[k,14],games[k,13])
      team.df[(k+1),17] <- team.df[(k),15] + team.df[(k+1),15]
      team.df[(k+1),18] <- team.df[(k),16] + team.df[(k+1),16]
      team.df[(k+1),19] <- ifelse(h_a == 1,games[k,15],games[k,16])
      team.df[(k+1),20] <- ifelse(h_a == 1,games[k,16],games[k,15])
      team.df[(k+1),21] <- team.df[(k),19] + team.df[(k+1),19]
      team.df[(k+1),22] <- team.df[(k),20] + team.df[(k+1),20]
      team.df[(k+1),23] <- ifelse(h_a == 1,games[k,17],games[k,18])
      team.df[(k+1),24] <- ifelse(h_a == 1,games[k,18],games[k,17])
      team.df[(k+1),25] <- team.df[(k),23] + team.df[(k+1),23]
      team.df[(k+1),26] <- team.df[(k),24] + team.df[(k+1),24]
      team.df[(k+1),27] <- ifelse(h_a == 1,games[k,19],games[k,20])
      team.df[(k+1),28] <- team.df[(k),27] + team.df[(k+1),27]
      team.df[(k+1),29] <- ifelse(h_a == 1,games[k,21],games[k,22])
      team.df[(k+1),30] <- team.df[(k),29] + team.df[(k+1),29]
    }
    team.df <- team.df[-1,]
    season.df <- rbind(season.df,team.df)
  }
  season.df <- season.df[-1,]
  season.df <- season.df[order(season.df[,3]),]
  EPL.aggregate <- rbind(EPL.aggregate,season.df)
}

EPL.aggregate <- EPL.aggregate[-1,]
colnames(EPL.aggregate) <- c("Season","Year","Week","Team","Opponent","Home_Away","Goals_Scored",
                             "Goals_Conceded","Points_Earned_Week","Points_Total","Shots_Taken",
                             "Shots_Conceded","Shots_Taken_Total","Shots_Conceded_Total","Shots_on_Target",
                             "Shots_on_Target_Conceded","Shots_on_Target_Total",
                             "Shots_on_Target_Conceded_Total",
                             "Fouls_Committed","Fouls_Against","Fouls_Committed_Total","Fouls_Against_Total",
                             "Corners","Corners_Conceded","Corners_Total","Corners_Conceded_Total",
                             "Yellow_Cards","Yellow_Cards_Total","Red_Cards","Red_Cards_Total")


write.csv(EPL.aggregate,file="Clean Data/EPL_aggregate.csv",row.names = FALSE)

head(EPL.aggregate)
```

    ##     Season Year Week        Team      Opponent Home_Away Goals_Scored
    ## 2    00/01 2000    1     Arsenal    Sunderland         A            0
    ## 210  00/01 2000    1 Aston Villa     Leicester         A            0
    ## 212  00/01 2000    1    Bradford     Liverpool         A            0
    ## 214  00/01 2000    1    Charlton      Man City         H            4
    ## 216  00/01 2000    1     Chelsea      West Ham         H            4
    ## 218  00/01 2000    1    Coventry Middlesbrough         H            1
    ##     Goals_Conceded Points_Earned_Week Points_Total Shots_Taken
    ## 2                1                  0            0          14
    ## 210              0                  1            1           5
    ## 212              1                  0            0           3
    ## 214              0                  3            3          17
    ## 216              2                  3            3          17
    ## 218              3                  0            0           6
    ##     Shots_Conceded Shots_Taken_Total Shots_Conceded_Total Shots_on_Target
    ## 2                8                14                    8               7
    ## 210              5                 5                    5               3
    ## 212             16                 3                   16               2
    ## 214              8                17                    8              14
    ## 216             12                17                   12              10
    ## 218             16                 6                   16               3
    ##     Shots_on_Target_Conceded Shots_on_Target_Total
    ## 2                          2                     7
    ## 210                        4                     3
    ## 212                       10                     2
    ## 214                        4                    14
    ## 216                        5                    10
    ## 218                        9                     3
    ##     Shots_on_Target_Conceded_Total Fouls_Committed Fouls_Against
    ## 2                                2              21            10
    ## 210                              4              12            12
    ## 212                             10               8             8
    ## 214                              4              13            12
    ## 216                              5              19            14
    ## 218                              9              15            21
    ##     Fouls_Committed_Total Fouls_Against_Total Corners Corners_Conceded
    ## 2                      21                  10       9                2
    ## 210                    12                  12       4                5
    ## 212                     8                   8       1                6
    ## 214                    13                  12       6                6
    ## 216                    19                  14       7                7
    ## 218                    15                  21       8                4
    ##     Corners_Total Corners_Conceded_Total Yellow_Cards Yellow_Cards_Total
    ## 2               9                      2            1                  1
    ## 210             4                      5            3                  3
    ## 212             1                      6            1                  1
    ## 214             6                      6            1                  1
    ## 216             7                      7            1                  1
    ## 218             8                      4            5                  5
    ##     Red_Cards Red_Cards_Total
    ## 2           1               1
    ## 210         0               0
    ## 212         0               0
    ## 214         0               0
    ## 216         0               0
    ## 218         1               1
