##################################################
### EPL Managers Data
##################################################

#For the current season 17/18
Name <- c("Arsene Wenger","Eddie Howe","Chris Hughton","Sean Dyche","Antonio Conte","Frank de Boer",
          "Roy Hodgson","David Unsworth","Ronald Koeman","Sam Allardyce","David Wagner","Craig Shakespeare",
          "Michael Appleton","Claude Puel","Jurgen Klopp","Josep Guardiola","Jose Mourinho","Rafael Benitez","Mauricio Pelligrino","Mark Hughes",
          "Paul Lambert","Eddie Niedzwiecki","Mark Hughes","Paul Clement","Carlos Carvalhal","Leon Britton","Mauricio Pochettino",
          "Javier Gracia","Marco Silva","Alan Pardew","Gary Megsan","Tony Pulis","David Moyes","Slavan Bilic")
Club <- c("Arsenal","Bournemouth","Brighton and Hove Albion","Burnley","Chelsea","Crystal Palace","Crystal Palace","Everton",
          "Everton","Everton","Huddersfield Town","Leicester City","Leicester City","Leicester City","Liverpool","Manchester City","Manchester United",
          "Newcastle","Southampton","Southampton","Southampton","Stoke City","Stoke City","Swansea","Swansea","Swansea",
          "Tottenham","Watford","Watford","West Bromwich Albion","West Bromwich Albion","West Bromwich Albion","West Ham United","West Ham United")
Start <- c()
End <- c(NA,NA,NA,NA,NA,"09/11/2017",NA,"11/30/2017","10/23/2017",NA,NA,"10/17/2017","10/23/2017",NA,NA,NA,NA,NA,
         "03/12/2018","01/06/2018",NA,"01/15/2018",NA, "12/20/2017", NA, "12/28/2017", NA, NA, "01/21/2018",NA, "11/29/2017","11/20/2017",NA,"11/06/2017")
End <- as.Date(End, format = "%m/%d/%Y")
DaysInCharge <- c()
Interim <- c(0,0,0,0,0,0,0,1,0,0,0,0,1,0,0,0,0,0,0,0,0,1,0,0,0,1,0,0,0,0,1,0,0,0)
Predecessor <- c()
Successor <- c()

Managers <- cbind.data.frame(Name,Club,End)