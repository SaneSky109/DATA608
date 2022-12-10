#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(tidyverse)
library(ggimage)
library(plotly)
library(magick)
library(rsconnect)
library(sjmisc)


# Data Source
# https://www.moneypuck.com/teams.htm
# https://www.moneypuck.com/data.htm

reg2017 <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/teams_2017_2018.csv")
reg2018 <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/teams_2018_2019.csv")
reg2019 <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/teams_2019_2020.csv")
reg2020 <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/teams_2020_2021.csv")
reg2021 <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/teams_2021_2022.csv")
reg2022 <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/teams_2022_2023.csv")

playoff2017 <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/teams_playoffs_2017_2018.csv")
playoff2018 <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/teams_playoffs_2018_2019.csv")
playoff2019 <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/teams_playoffs_2019_2020.csv")
playoff2020 <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/teams_playoffs_2020_2021.csv")
playoff2021 <- read.csv("https://raw.githubusercontent.com/SaneSky109/DATA608/main/Final_Project/Data/teams_playoffs_2021_2022.csv")



# preprocess

reg2017$team[reg2017$team == "N.J"] <- "NJD"
reg2018$team[reg2018$team == "N.J"] <- "NJD"
reg2019$team[reg2019$team == "N.J"] <- "NJD"
reg2020$team[reg2020$team == "N.J"] <- "NJD"
reg2021$team[reg2021$team == "N.J"] <- "NJD"

reg2017$team[reg2017$team == "S.J"] <- "SJS"
reg2018$team[reg2018$team == "S.J"] <- "SJS"
reg2019$team[reg2019$team == "S.J"] <- "SJS"
reg2020$team[reg2020$team == "S.J"] <- "SJS"
reg2021$team[reg2021$team == "S.J"] <- "SJS"

reg2017$team[reg2017$team == "L.A"] <- "LAK"
reg2018$team[reg2018$team == "L.A"] <- "LAK"
reg2019$team[reg2019$team == "L.A"] <- "LAK"
reg2020$team[reg2020$team == "L.A"] <- "LAK"
reg2021$team[reg2021$team == "L.A"] <- "LAK"

reg2017$team[reg2017$team == "T.B"] <- "TBL"
reg2018$team[reg2018$team == "T.B"] <- "TBL"
reg2019$team[reg2019$team == "T.B"] <- "TBL"
reg2020$team[reg2020$team == "T.B"] <- "TBL"
reg2021$team[reg2021$team == "T.B"] <- "TBL"


playoff2017$team[playoff2017$team == "N.J"] <- "NJD"
playoff2018$team[playoff2018$team == "N.J"] <- "NJD"
playoff2019$team[playoff2019$team == "N.J"] <- "NJD"
playoff2020$team[playoff2020$team == "N.J"] <- "NJD"
playoff2021$team[playoff2021$team == "N.J"] <- "NJD"

playoff2017$team[playoff2017$team == "S.J"] <- "SJS"
playoff2018$team[playoff2018$team == "S.J"] <- "SJS"
playoff2019$team[playoff2019$team == "S.J"] <- "SJS"
playoff2020$team[playoff2020$team == "S.J"] <- "SJS"
playoff2021$team[playoff2021$team == "S.J"] <- "SJS"

playoff2017$team[playoff2017$team == "L.A"] <- "LAK"
playoff2018$team[playoff2018$team == "L.A"] <- "LAK"
playoff2019$team[playoff2019$team == "L.A"] <- "LAK"
playoff2020$team[playoff2020$team == "L.A"] <- "LAK"
playoff2021$team[playoff2021$team == "L.A"] <- "LAK"

playoff2017$team[playoff2017$team == "T.B"] <- "TBL"
playoff2018$team[playoff2018$team == "T.B"] <- "TBL"
playoff2019$team[playoff2019$team == "T.B"] <- "TBL"
playoff2020$team[playoff2020$team == "T.B"] <- "TBL"
playoff2021$team[playoff2021$team == "T.B"] <- "TBL"


playoff2021.teams <- unique(playoff2021$team)
playoff2020.teams <- unique(playoff2020$team)
playoff2019.teams <- unique(playoff2019$team)
playoff2018.teams <- unique(playoff2018$team)
playoff2017.teams <- unique(playoff2017$team)

reg2017$playoffFlag <- NA
reg2017$playoffFlag <- ifelse(reg2017$team %in% playoff2017.teams, "In Playoffs", "Not in Playoffs")
reg2018$playoffFlag <- NA
reg2018$playoffFlag <- ifelse(reg2018$team %in% playoff2018.teams, "In Playoffs", "Not in Playoffs")
reg2019$playoffFlag <- NA
reg2019$playoffFlag <- ifelse(reg2019$team %in% playoff2019.teams, "In Playoffs", "Not in Playoffs")
reg2020$playoffFlag <- NA
reg2020$playoffFlag <- ifelse(reg2020$team %in% playoff2020.teams, "In Playoffs", "Not in Playoffs")
reg2021$playoffFlag <- NA
reg2021$playoffFlag <- ifelse(reg2021$team %in% playoff2021.teams, "In Playoffs", "Not in Playoffs")



metro <- c("CAR", "CBJ", "NJD", "NYI", "NYR", "PHI", "PIT", "WSH")
atlantic <- c("BOS", "BUF", "DET", "FLA", "MTL", "OTT", "TBL", "TOR")
central <- c("ARI", "CHI", "COL", "DAL", "MIN", "NSH", "STL", "WPG")
pacific <- c("ANA", "CGY", "EDM", "LAK", "SJS", "SEA", "VAN", "VGK")

# metro
for(i in 1:length(metro)){
    reg2017$division[reg2017$team == metro[i]] <- "Metropolitan"
}

for(i in 1:length(metro)){
    reg2018$division[reg2018$team == metro[i]] <- "Metropolitan"
}

for(i in 1:length(metro)){
    reg2019$division[reg2019$team == metro[i]] <- "Metropolitan"
}

for(i in 1:length(metro)){
    reg2020$division[reg2020$team == metro[i]] <- "Metropolitan"
}

for(i in 1:length(metro)){
    reg2021$division[reg2021$team == metro[i]] <- "Metropolitan"
}

for(i in 1:length(metro)){
    playoff2017$division[playoff2017$team == metro[i]] <- "Metropolitan"
}

for(i in 1:length(metro)){
    playoff2018$division[playoff2018$team == metro[i]] <- "Metropolitan"
}

for(i in 1:length(metro)){
    playoff2019$division[playoff2019$team == metro[i]] <- "Metropolitan"
}

for(i in 1:length(metro)){
    playoff2020$division[playoff2020$team == metro[i]] <- "Metropolitan"
}

for(i in 1:length(metro)){
    playoff2021$division[playoff2021$team == metro[i]] <- "Metropolitan"
}

# Atlantic
for(i in 1:length(atlantic)){
    reg2017$division[reg2017$team == atlantic[i]] <- "Atlantic"
}

for(i in 1:length(atlantic)){
    reg2018$division[reg2018$team == atlantic[i]] <- "Atlantic"
}

for(i in 1:length(atlantic)){
    reg2019$division[reg2019$team == atlantic[i]] <- "Atlantic"
}

for(i in 1:length(atlantic)){
    reg2020$division[reg2020$team == atlantic[i]] <- "Atlantic"
}

for(i in 1:length(atlantic)){
    reg2021$division[reg2021$team == atlantic[i]] <- "Atlantic"
}

for(i in 1:length(atlantic)){
    playoff2017$division[playoff2017$team == atlantic[i]] <- "Atlantic"
}

for(i in 1:length(atlantic)){
    playoff2018$division[playoff2018$team == atlantic[i]] <- "Atlantic"
}

for(i in 1:length(atlantic)){
    playoff2019$division[playoff2019$team == atlantic[i]] <- "Atlantic"
}

for(i in 1:length(atlantic)){
    playoff2020$division[playoff2020$team == atlantic[i]] <- "Atlantic"
}

for(i in 1:length(atlantic)){
    playoff2021$division[playoff2021$team == atlantic[i]] <- "Atlantic"
}

# Central
for(i in 1:length(central)){
    reg2017$division[reg2017$team == central[i]] <- "Central"
}

for(i in 1:length(central)){
    reg2018$division[reg2018$team == central[i]] <- "Central"
}

for(i in 1:length(central)){
    reg2019$division[reg2019$team == central[i]] <- "Central"
}

for(i in 1:length(central)){
    reg2020$division[reg2020$team == central[i]] <- "Central"
}

for(i in 1:length(central)){
    reg2021$division[reg2021$team == central[i]] <- "Central"
}

for(i in 1:length(central)){
    playoff2017$division[playoff2017$team == central[i]] <- "Central"
}

for(i in 1:length(central)){
    playoff2018$division[playoff2018$team == central[i]] <- "Central"
}

for(i in 1:length(central)){
    playoff2019$division[playoff2019$team == central[i]] <- "Central"
}

for(i in 1:length(central)){
    playoff2020$division[playoff2020$team == central[i]] <- "Central"
}

for(i in 1:length(central)){
    playoff2021$division[playoff2021$team == central[i]] <- "Central"
}

# Pacific
for(i in 1:length(pacific)){
    reg2017$division[reg2017$team == pacific[i]] <- "Pacific"
}

for(i in 1:length(pacific)){
    reg2018$division[reg2018$team == pacific[i]] <- "Pacific"
}

for(i in 1:length(pacific)){
    reg2019$division[reg2019$team == pacific[i]] <- "Pacific"
}

for(i in 1:length(pacific)){
    reg2020$division[reg2020$team == pacific[i]] <- "Pacific"
}

for(i in 1:length(pacific)){
    reg2021$division[reg2021$team == pacific[i]] <- "Pacific"
}

for(i in 1:length(pacific)){
    playoff2017$division[playoff2017$team == pacific[i]] <- "Pacific"
}

for(i in 1:length(pacific)){
    playoff2018$division[playoff2018$team == pacific[i]] <- "Pacific"
}

for(i in 1:length(pacific)){
    playoff2019$division[playoff2019$team == pacific[i]] <- "Pacific"
}

for(i in 1:length(pacific)){
    playoff2020$division[playoff2020$team == pacific[i]] <- "Pacific"
}

for(i in 1:length(pacific)){
    playoff2021$division[playoff2021$team == pacific[i]] <- "Pacific"
}

# playoff distance
elimFirstRound2017 <- c("COL", "MIN", "LAK", "ANA", "NJD", "TOR", "CBJ", "PHI")
elimSecondRound2017 <- c("NSH", "SJS", "BOS", "PIT")
elimThirdRound2017 <- c("WPG", "TBL")
elimFinals2017 <- c("VGK")
champion2017 <- c("WSH")


for(i in 1:length(elimFirstRound2017)){
    playoff2017$distance[playoff2017$team == elimFirstRound2017[i]] <- "Eliminated in Round 1"
}

for(i in 1:length(elimSecondRound2017)){
    playoff2017$distance[playoff2017$team == elimSecondRound2017[i]] <- "Eliminated in Round 2"
}

for(i in 1:length(elimThirdRound2017)){
    playoff2017$distance[playoff2017$team == elimThirdRound2017[i]] <- "Eliminated in Round 3"
}

for(i in 1:length(elimFinals2017)){
    playoff2017$distance[playoff2017$team == elimFinals2017 [i]] <- "Eliminated in Stanley Cup Finals"
}

for(i in 1:length(champion2017)){
    playoff2017$distance[playoff2017$team == champion2017[i]] <- "Won Stanley Cup Finals"
}


elimFirstRound2018 <- c("NSH", "WPG", "CGY", "VGK", "TBL", "TOR", "WSH", "PIT")
elimSecondRound2018 <- c("DAL", "COL", "CBJ", "NYI")
elimThirdRound2018 <- c("SJS", "CAR")
elimFinals2018 <- c("BOS")
champion2018 <- c("STL")


for(i in 1:length(elimFirstRound2018)){
    playoff2018$distance[playoff2018$team == elimFirstRound2018[i]] <- "Eliminated in Round 1"
}

for(i in 1:length(elimSecondRound2018)){
    playoff2018$distance[playoff2018$team == elimSecondRound2018[i]] <- "Eliminated in Round 2"
}

for(i in 1:length(elimThirdRound2018)){
    playoff2018$distance[playoff2018$team == elimThirdRound2018[i]] <- "Eliminated in Round 3"
}

for(i in 1:length(elimFinals2018)){
    playoff2018$distance[playoff2018$team == elimFinals2018 [i]] <- "Eliminated in Stanley Cup Finals"
}

for(i in 1:length(champion2018)){
    playoff2018$distance[playoff2018$team == champion2018[i]] <- "Won Stanley Cup Finals"
}

elimRoundRobin <- c("NYR", "FLA", "MIN", "PIT", "WPG", "TOR", "EDM", "NSH", "VAN")
elimFirstRound2019 <- c("CHI", "ARI", "CGY", "STL", "MTL", "CBJ", "WSH", "CAR")
elimSecondRound2019 <- c("SJS", "COL", "PHI", "BOS")
elimThirdRound2019 <- c("VGK", "NYI")
elimFinals2019 <- c("DAL")
champion2019 <- c("TBL")


for(i in 1:length(elimFirstRound2019)){
    playoff2019$distance[playoff2019$team == elimFirstRound2019[i]] <- "Eliminated in Round 1"
}

for(i in 1:length(elimSecondRound2019)){
    playoff2019$distance[playoff2019$team == elimSecondRound2019[i]] <- "Eliminated in Round 2"
}

for(i in 1:length(elimThirdRound2019)){
    playoff2019$distance[playoff2019$team == elimThirdRound2019[i]] <- "Eliminated in Round 3"
}

for(i in 1:length(elimFinals2019)){
    playoff2019$distance[playoff2019$team == elimFinals2019 [i]] <- "Eliminated in Stanley Cup Finals"
}

for(i in 1:length(champion2019)){
    playoff2019$distance[playoff2019$team == champion2019[i]] <- "Won Stanley Cup Finals"
}

for(i in 1:length(elimRoundRobin)){
    playoff2019$distance[playoff2019$team == elimRoundRobin[i]] <- "Eliminated in Round Robin"
}


elimFirstRound2020 <- c("STL", "MIN", "TOR", "EDM", "PIT", "WSH", "NSH", "FLA")
elimSecondRound2020 <- c("COL", "WPG", "BOS", "CAR")
elimThirdRound2020 <- c("VGK", "NYI")
elimFinals2020 <- c("MTL")
champion2020 <- c("TBL")


for(i in 1:length(elimFirstRound2020)){
    playoff2020$distance[playoff2020$team == elimFirstRound2020[i]] <- "Eliminated in Round 1"
}

for(i in 1:length(elimSecondRound2020)){
    playoff2020$distance[playoff2020$team == elimSecondRound2020[i]] <- "Eliminated in Round 2"
}

for(i in 1:length(elimThirdRound2020)){
    playoff2020$distance[playoff2020$team == elimThirdRound2020[i]] <- "Eliminated in Round 3"
}

for(i in 1:length(elimFinals2020)){
    playoff2020$distance[playoff2020$team == elimFinals2020 [i]] <- "Eliminated in Stanley Cup Finals"
}

for(i in 1:length(champion2020)){
    playoff2020$distance[playoff2020$team == champion2020[i]] <- "Won Stanley Cup Finals"
}


elimFirstRound2021 <- c("NSH", "MIN", "DAL", "LAK", "WSH", "TOR", "BOS", "PIT")
elimSecondRound2021 <- c("STL", "CGY", "FLA", "CAR")
elimThirdRound2021 <- c("EDM", "NYR")
elimFinals2021 <- c("TBL")
champion2021 <- c("COL")


for(i in 1:length(elimFirstRound2021)){
    playoff2021$distance[playoff2021$team == elimFirstRound2021[i]] <- "Eliminated in Round 1"
}

for(i in 1:length(elimSecondRound2021)){
    playoff2021$distance[playoff2021$team == elimSecondRound2021[i]] <- "Eliminated in Round 2"
}

for(i in 1:length(elimThirdRound2021)){
    playoff2021$distance[playoff2021$team == elimThirdRound2021[i]] <- "Eliminated in Round 3"
}

for(i in 1:length(elimFinals2021)){
    playoff2021$distance[playoff2021$team == elimFinals2021 [i]] <- "Eliminated in Stanley Cup Finals"
}

for(i in 1:length(champion2021)){
    playoff2021$distance[playoff2021$team == champion2021[i]] <- "Won Stanley Cup Finals"
}


# rbind all datasets

reg2017$faceOffPercentage <- reg2017$faceOffsWonFor / (reg2017$faceOffsWonFor + reg2017$faceOffsWonAgainst)
reg2018$faceOffPercentage <- reg2018$faceOffsWonFor / (reg2018$faceOffsWonFor + reg2018$faceOffsWonAgainst)
reg2019$faceOffPercentage <- reg2019$faceOffsWonFor / (reg2019$faceOffsWonFor + reg2019$faceOffsWonAgainst)
reg2020$faceOffPercentage <- reg2020$faceOffsWonFor / (reg2020$faceOffsWonFor + reg2020$faceOffsWonAgainst)
reg2021$faceOffPercentage <- reg2021$faceOffsWonFor / (reg2021$faceOffsWonFor + reg2021$faceOffsWonAgainst)

playoff2017$faceOffPercentage <- playoff2017$faceOffsWonFor / (playoff2017$faceOffsWonFor + playoff2017$faceOffsWonAgainst)
playoff2018$faceOffPercentage <- playoff2018$faceOffsWonFor / (playoff2018$faceOffsWonFor + playoff2018$faceOffsWonAgainst)
playoff2019$faceOffPercentage <- playoff2019$faceOffsWonFor / (playoff2019$faceOffsWonFor + playoff2019$faceOffsWonAgainst)
playoff2020$faceOffPercentage <- playoff2020$faceOffsWonFor / (playoff2020$faceOffsWonFor + playoff2020$faceOffsWonAgainst)
playoff2021$faceOffPercentage <- playoff2021$faceOffsWonFor / (playoff2021$faceOffsWonFor + playoff2021$faceOffsWonAgainst)

reg2017$savePercentage <- (reg2017$shotsOnGoalAgainst - reg2017$goalsAgainst) / reg2017$shotsOnGoalAgainst
reg2018$savePercentage <- (reg2018$shotsOnGoalAgainst - reg2018$goalsAgainst) / reg2018$shotsOnGoalAgainst
reg2019$savePercentage <- (reg2019$shotsOnGoalAgainst - reg2019$goalsAgainst) / reg2019$shotsOnGoalAgainst
reg2020$savePercentage <- (reg2020$shotsOnGoalAgainst - reg2020$goalsAgainst) / reg2020$shotsOnGoalAgainst
reg2021$savePercentage <- (reg2021$shotsOnGoalAgainst - reg2021$goalsAgainst) / reg2021$shotsOnGoalAgainst

playoff2017$savePercentage <- (playoff2017$shotsOnGoalAgainst - playoff2017$goalsAgainst) / playoff2017$shotsOnGoalAgainst
playoff2018$savePercentage <- (playoff2018$shotsOnGoalAgainst - playoff2018$goalsAgainst) / playoff2018$shotsOnGoalAgainst
playoff2019$savePercentage <- (playoff2019$shotsOnGoalAgainst - playoff2019$goalsAgainst) / playoff2019$shotsOnGoalAgainst
playoff2020$savePercentage <- (playoff2020$shotsOnGoalAgainst - playoff2020$goalsAgainst) / playoff2020$shotsOnGoalAgainst
playoff2021$savePercentage <- (playoff2021$shotsOnGoalAgainst - playoff2021$goalsAgainst) / playoff2021$shotsOnGoalAgainst




all.data <- bind_rows(reg2017, reg2018, reg2019, reg2020, reg2021)
all.data.playoff <- bind_rows(playoff2017, playoff2018, playoff2019, playoff2020, playoff2021)



selected.vars.reg <- c("team", "season", "name", "situation", "games_played", "playoffFlag", "division", "xGoalsPercentage", "corsiPercentage", "fenwickPercentage", "flurryAdjustedxGoalsFor", "flurryAdjustedxGoalsAgainst", "blockedShotAttemptsFor", "blockedShotAttemptsAgainst", "shotsOnGoalFor", "shotsOnGoalAgainst", "missedShotsFor", "missedShotsAgainst", "goalsFor", "goalsAgainst", "reboundsFor", "reboundsAgainst", "reboundGoalsFor", "reboundGoalsAgainst", "freezeFor", "freezeAgainst", "savedShotsOnGoalFor", "savedShotsOnGoalAgainst", "penaltiesFor", "penaltiesAgainst", "faceOffPercentage", "hitsFor", "hitsAgainst", "takeawaysFor", "takeawaysAgainst", "giveawaysFor", "giveawaysAgainst", "lowDangerShotsFor", "lowDangerShotsAgainst", "mediumDangerShotsFor", "mediumDangerShotsAgainst", "highDangerShotsFor", "highDangerShotsAgainst", "savePercentage")
selected.vars.playoff <- c("team", "season", "name", "situation", "games_played", "distance", "division", "xGoalsPercentage", "corsiPercentage", "fenwickPercentage", "flurryAdjustedxGoalsFor", "flurryAdjustedxGoalsAgainst", "blockedShotAttemptsFor", "blockedShotAttemptsAgainst", "shotsOnGoalFor", "shotsOnGoalAgainst", "missedShotsFor", "missedShotsAgainst", "goalsFor", "goalsAgainst", "reboundsFor", "reboundsAgainst", "reboundGoalsFor", "reboundGoalsAgainst", "freezeFor", "freezeAgainst", "savedShotsOnGoalFor", "savedShotsOnGoalAgainst", "penaltiesFor", "penaltiesAgainst", "faceOffPercentage", "hitsFor", "hitsAgainst", "takeawaysFor", "takeawaysAgainst", "giveawaysFor", "giveawaysAgainst", "lowDangerShotsFor", "lowDangerShotsAgainst", "mediumDangerShotsFor", "mediumDangerShotsAgainst", "highDangerShotsFor", "highDangerShotsAgainst", "savePercentage")

all.data.select <- all.data[,selected.vars.reg]
all.data.playoff.select <- all.data.playoff[,selected.vars.playoff]

all.data.select$season <- as.factor(all.data.select$season)
all.data.playoff.select$season <- as.factor(all.data.playoff.select$season)

all.data.select$flurryAdjustedxGoalsFor <- all.data.select$flurryAdjustedxGoalsFor / all.data.select$games_played
all.data.select$flurryAdjustedxGoalsAgainst <- all.data.select$flurryAdjustedxGoalsAgainst / all.data.select$games_played
all.data.select$blockedShotAttemptsFor <- all.data.select$blockedShotAttemptsFor / all.data.select$games_played
all.data.select$blockedShotAttemptsAgainst <- all.data.select$blockedShotAttemptsAgainst / all.data.select$games_played
all.data.select$highDangerShotsFor <- all.data.select$highDangerShotsFor  / all.data.select$games_played
all.data.select$highDangerShotsAgainst <- all.data.select$highDangerShotsAgainst / all.data.select$games_played
all.data.select$mediumDangerShotsFor <- all.data.select$mediumDangerShotsFor  / all.data.select$games_played
all.data.select$mediumDangerShotsAgainst <- all.data.select$mediumDangerShotsAgainst / all.data.select$games_played
all.data.select$lowDangerShotsFor <- all.data.select$lowDangerShotsFor / all.data.select$games_played
all.data.select$lowDangerShotsAgainst <- all.data.select$lowDangerShotsAgainst / all.data.select$games_played
all.data.select$giveawaysFor <- all.data.select$giveawaysFor / all.data.select$games_played
all.data.select$giveawaysAgainst <- all.data.select$giveawaysAgainst / all.data.select$games_played
all.data.select$takeawaysFor <- all.data.select$takeawaysFor / all.data.select$games_played
all.data.select$takeawaysAgainst <- all.data.select$takeawaysAgainst / all.data.select$games_played
all.data.select$hitsFor <- all.data.select$hitsFor / all.data.select$games_played
all.data.select$hitsAgainst <- all.data.select$hitsAgainst / all.data.select$games_played
all.data.select$penaltiesFor <- all.data.select$penaltiesFor / all.data.select$games_played
all.data.select$penaltiesAgainst <- all.data.select$penaltiesAgainst / all.data.select$games_played
all.data.select$savedShotsOnGoalFor <- all.data.select$savedShotsOnGoalFor  / all.data.select$games_played
all.data.select$savedShotsOnGoalAgainst <- all.data.select$savedShotsOnGoalAgainst / all.data.select$games_played
all.data.select$freezeFor <- all.data.select$freezeFor / all.data.select$games_played
all.data.select$freezeAgainst <- all.data.select$freezeAgainst / all.data.select$games_played
all.data.select$reboundGoalsFor <- all.data.select$reboundGoalsFor / all.data.select$games_played
all.data.select$shotsOnGoalFor <- all.data.select$shotsOnGoalFor / all.data.select$games_played
all.data.select$shotsOnGoalAgainst <- all.data.select$shotsOnGoalAgainst / all.data.select$games_played
all.data.select$missedShotsFor <- all.data.select$missedShotsFor / all.data.select$games_played
all.data.select$missedShotsAgainst <- all.data.select$missedShotsAgainst / all.data.select$games_played
all.data.select$goalsFor <- all.data.select$goalsFor / all.data.select$games_played
all.data.select$goalsAgainst <- all.data.select$goalsAgainst / all.data.select$games_played
all.data.select$reboundsFor <- all.data.select$reboundsFor / all.data.select$games_played
all.data.select$reboundsAgainst <- all.data.select$reboundsAgainst / all.data.select$games_played

all.data.playoff.select$flurryAdjustedxGoalsFor <- all.data.playoff.select$flurryAdjustedxGoalsFor / all.data.playoff.select$games_played
all.data.playoff.select$flurryAdjustedxGoalsAgainst <- all.data.playoff.select$flurryAdjustedxGoalsAgainst / all.data.playoff.select$games_played
all.data.playoff.select$blockedShotAttemptsFor <- all.data.playoff.select$blockedShotAttemptsFor / all.data.playoff.select$games_played
all.data.playoff.select$blockedShotAttemptsAgainst <- all.data.playoff.select$blockedShotAttemptsAgainst / all.data.playoff.select$games_played
all.data.playoff.select$highDangerShotsFor <- all.data.playoff.select$highDangerShotsFor  / all.data.playoff.select$games_played
all.data.playoff.select$highDangerShotsAgainst <- all.data.playoff.select$highDangerShotsAgainst / all.data.playoff.select$games_played
all.data.playoff.select$mediumDangerShotsFor <- all.data.playoff.select$mediumDangerShotsFor  / all.data.playoff.select$games_played
all.data.playoff.select$mediumDangerShotsAgainst <- all.data.playoff.select$mediumDangerShotsAgainst / all.data.playoff.select$games_played
all.data.playoff.select$lowDangerShotsFor <- all.data.playoff.select$lowDangerShotsFor / all.data.playoff.select$games_played
all.data.playoff.select$lowDangerShotsAgainst <- all.data.playoff.select$lowDangerShotsAgainst / all.data.playoff.select$games_played
all.data.playoff.select$giveawaysFor <- all.data.playoff.select$giveawaysFor / all.data.playoff.select$games_played
all.data.playoff.select$giveawaysAgainst <- all.data.playoff.select$giveawaysAgainst / all.data.playoff.select$games_played
all.data.playoff.select$takeawaysFor <- all.data.playoff.select$takeawaysFor / all.data.playoff.select$games_played
all.data.playoff.select$takeawaysAgainst <- all.data.playoff.select$takeawaysAgainst / all.data.playoff.select$games_played
all.data.playoff.select$hitsFor <- all.data.playoff.select$hitsFor / all.data.playoff.select$games_played
all.data.playoff.select$hitsAgainst <- all.data.playoff.select$hitsAgainst / all.data.playoff.select$games_played
all.data.playoff.select$penaltiesFor <- all.data.playoff.select$penaltiesFor / all.data.playoff.select$games_played
all.data.playoff.select$penaltiesAgainst <- all.data.playoff.select$penaltiesAgainst / all.data.playoff.select$games_played
all.data.playoff.select$savedShotsOnGoalFor <- all.data.playoff.select$savedShotsOnGoalFor  / all.data.playoff.select$games_played
all.data.playoff.select$savedShotsOnGoalAgainst <- all.data.playoff.select$savedShotsOnGoalAgainst / all.data.playoff.select$games_played
all.data.playoff.select$freezeFor <- all.data.playoff.select$freezeFor / all.data.playoff.select$games_played
all.data.playoff.select$freezeAgainst <- all.data.playoff.select$freezeAgainst / all.data.playoff.select$games_played
all.data.playoff.select$reboundGoalsFor <- all.data.playoff.select$reboundGoalsFor / all.data.playoff.select$games_played
all.data.playoff.select$shotsOnGoalFor <- all.data.playoff.select$shotsOnGoalFor / all.data.playoff.select$games_played
all.data.playoff.select$shotsOnGoalAgainst <- all.data.playoff.select$shotsOnGoalAgainst / all.data.playoff.select$games_played
all.data.playoff.select$missedShotsFor <- all.data.playoff.select$missedShotsFor / all.data.playoff.select$games_played
all.data.playoff.select$missedShotsAgainst <- all.data.playoff.select$missedShotsAgainst / all.data.playoff.select$games_played
all.data.playoff.select$goalsFor <- all.data.playoff.select$goalsFor / all.data.playoff.select$games_played
all.data.playoff.select$goalsAgainst <- all.data.playoff.select$goalsAgainst / all.data.playoff.select$games_played
all.data.playoff.select$reboundsFor <- all.data.playoff.select$reboundsFor / all.data.playoff.select$games_played
all.data.playoff.select$reboundsAgainst <- all.data.playoff.select$reboundsAgainst / all.data.playoff.select$games_played


var.names1 <- c("xGoalsPercentage", "corsiPercentage", "fenwickPercentage", "flurryAdjustedxGoalsFor", "flurryAdjustedxGoalsAgainst", "blockedShotAttemptsFor", "blockedShotAttemptsAgainst", "shotsOnGoalFor", "shotsOnGoalAgainst", "missedShotsFor", "missedShotsAgainst", "goalsFor", "goalsAgainst", "reboundsFor", "reboundsAgainst", "reboundGoalsFor", "reboundGoalsAgainst", "freezeFor", "freezeAgainst", "savedShotsOnGoalFor", "savedShotsOnGoalAgainst", "penaltiesFor", "penaltiesAgainst", "faceOffPercentage", "hitsFor", "hitsAgainst", "takeawaysFor", "takeawaysAgainst", "giveawaysFor", "giveawaysAgainst", "lowDangerShotsFor", "lowDangerShotsAgainst", "mediumDangerShotsFor", "mediumDangerShotsAgainst", "highDangerShotsFor", "highDangerShotsAgainst", "savePercentage")
var.names <- names(all.data.select)

t <- list(size = 12)


# function to draw horizontal line
hline <- function(y = 0, color = "black") {
    list(
        type = "line",
        x0 = 0,
        x1 = 1,
        xref = "paper",
        y0 = y,
        y1 = y,
        line = list(color = color)
    )
}




# Define UI for application that draws a histogram
ui <- fluidPage(
    
    navbarPage(theme = shinytheme("darkly"),
               "NHL Stats Analysis: 2017 - 2022",
               # tab containing project information
               tabPanel("Information",
                        fluidPage(
                            includeMarkdown("Information.rmd")
                        )), 
               # tab containing main Regular season data
               tabPanel("Regular Season", 
                        fluidPage(
                            
                            fluidRow(
                                # Set up Season Selection Button
                                column(12, align =  "center",
                                       h2("Input for Both Plots"),
                                       radioButtons(inputId = "Season", 
                                                    label = "Select Season",
                                                    choices = unique(all.data.select$season),
                                                    selected = "2017",
                                                    inline = TRUE),
                                       #column(6, align = "center",
                                       # Set up Situation Selection Button
                                       radioButtons(inputId = "Situation", 
                                                    label = "Select Situation",
                                                    choices = unique(all.data.select$situation),
                                                    selected = "all", 
                                                    inline = TRUE)
                                )),
                            
                            fluidRow(
                                column(2,
                                       h2("Left Plot Input"),
                                       # Set up Season Selection Button
                                       # selectInput(inputId = "Season", 
                                       #             label = "Select Season",
                                       #             choices = all.data$season,
                                       #             selected = "2017"),
                                       # # Set up Situation Selection Button
                                       # selectInput(inputId = "Situation", 
                                       #             label = "Select Situation",
                                       #             choices = all.data$situation,
                                       #             selected = "all"),
                                       # Set up Variable Selection Button
                                       selectInput(inputId = "y_var1", 
                                                   label = "Select Variable",
                                                   choices = var.names1,
                                                   selected = "xGoalsPercentage")
                                       
                                ), column(4,
                                          plotlyOutput("regPlot1", height = 800)
                                ), column(4,
                                          plotlyOutput("regPlot2", height = 800)
                                ), column(2,
                                          h2("Right Plot Input"),
                                          # Set up Plot 2 Variable x Selection Button
                                          selectInput(inputId = "x_var2", 
                                                      label = "Select X Axis Variable",
                                                      choices = var.names1,
                                                      selected = "xGoalsPercentage"),
                                          # Set up Plot 2 Variable y Selection Button
                                          selectInput(inputId = "y_var2", 
                                                      label = "Select Y Axis Variable",
                                                      choices = var.names1,
                                                      selected = "xGoalsPercentage")) 
                                
                            )
                        )
                        
               ),
               # tab containing Regular season Team data
               tabPanel("Regular Season: Timeseries", 
                        fluidPage(
                            column(12, align =  "center",
                                   h2("Input"),
                                   
                                   selectInput(inputId = "team", 
                                               label = "Select Team",
                                               choices = unique(all.data.select$team),
                                               selected = "NJD"),
                                   # Set up Situation Selection Button
                                   radioButtons(inputId = "Situation2", 
                                                label = "Select Situation",
                                                choices = unique(all.data.select$situation),
                                                selected = "all", 
                                                inline = TRUE),
                                   selectInput(inputId = "y_var3", 
                                               label = "Select Variable",
                                               choices = var.names1,
                                               selected = "xGoalsPercentage")
                            ),
                            fluidRow(column(12, align =  "center",
                                            plotlyOutput("regPlot3", height = 700)
                            )))),
               # tab containing Playoff data
               tabPanel("Playoffs", 
                        fluidPage(
                            
                            fluidRow(
                                # Set up Season Selection Button
                                column(12, align =  "center",
                                       h2("Input for Both Plots"),
                                       radioButtons(inputId = "Season3", 
                                                    label = "Select Season",
                                                    choices = unique(all.data.select$season),
                                                    selected = "2017",
                                                    inline = TRUE),
                                       #column(6, align = "center",
                                       # Set up Situation Selection Button
                                       radioButtons(inputId = "Situation3", 
                                                    label = "Select Situation",
                                                    choices = unique(all.data.select$situation),
                                                    selected = "all", 
                                                    inline = TRUE)
                                )),
                            
                            fluidRow(
                                column(2,
                                       h2("Left Plot Input"),
                                       # Set up Season Selection Button
                                       # selectInput(inputId = "Season", 
                                       #             label = "Select Season",
                                       #             choices = all.data$season,
                                       #             selected = "2017"),
                                       # # Set up Situation Selection Button
                                       # selectInput(inputId = "Situation", 
                                       #             label = "Select Situation",
                                       #             choices = all.data$situation,
                                       #             selected = "all"),
                                       # Set up Variable Selection Button
                                       selectInput(inputId = "y_var4", 
                                                   label = "Select Variable",
                                                   choices = var.names1,
                                                   selected = "xGoalsPercentage")
                                       
                                ), column(4,
                                          plotlyOutput("playoffPlot1", height = 800)
                                ), column(4,
                                          plotlyOutput("playoffPlot2", height = 800)
                                ), column(2,
                                          h2("Right Plot Input"),
                                          # Set up Plot 2 Variable x Selection Button
                                          selectInput(inputId = "x_var5", 
                                                      label = "Select X Axis Variable",
                                                      choices = var.names1,
                                                      selected = "xGoalsPercentage"),
                                          # Set up Plot 2 Variable y Selection Button
                                          selectInput(inputId = "y_var5", 
                                                      label = "Select Y Axis Variable",
                                                      choices = var.names1,
                                                      selected = "xGoalsPercentage")) 
                                
                            )
                        )
                        
               ),
               # tab containing takeaways
               tabPanel("Conclusion", 
                        fluidPage(
                            includeMarkdown("Conclusion.rmd")
                        )
               ),
               
               
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Reg season plot 1
    
    # Make data reactive
    all_data_reactive <- reactive(all.data.select %>%
                                      filter(season == input$Season) %>%
                                      filter(situation == input$Situation))
    
    historic.avg_reactive <- reactive(all.data.select %>%
                                          filter(situation == input$Situation)) 
    
    historic.avg.playoff_reactive <- reactive(all.data.playoff.select %>%
                                                  filter(situation == input$Situation3)) 
    
    line.data_reactive <- reactive(all.data.select %>%
                                       filter(team == input$team) %>%
                                       filter(situation == input$Situation2))
    
    all.data.playoff_reactive <- reactive(all.data.playoff.select %>%
                                              filter(season == input$Season3) %>%
                                              filter(situation == input$Situation3))
    
    # Create plotly graph
    output$regPlot1 <- renderPlotly({
        plot_ly(
            data = all_data_reactive(),
            x = ~team,
            y = as.formula(paste0('~', input$y_var1)),
            type = "bar",
            color = ~playoffFlag,
            colors = c('#D37300', "#00126A")
        ) %>%
            layout(title = paste0('Seasonal ', input$y_var1, ' Compared to NHL Average from 2017 to 2022'), font = t,
                   shapes = list(hline(mean(historic.avg_reactive()[,var.names == input$y_var1]), "red")),
                   xaxis = list(categoryorder = "total ascending"))
    })
    
    
    # reg season  plot 2
    output$regPlot2 <- renderPlotly({
        plot_ly(
            data = all_data_reactive(),
            x = as.formula(paste0('~', input$x_var2)),
            y = as.formula(paste0('~', input$y_var2)),
            type = "scatter",
            mode = "markers",
            color = ~playoffFlag,
            colors = c('#D37300', "#00126A"),
            size = 10
        )%>%
            layout(title = paste0('NHL Stats: ', input$x_var2, " vs. ", input$y_var2), font = t)
    })
    
    
    # reg season plot 3
    output$regPlot3 <- renderPlotly({
        plot_ly(
            data = line.data_reactive(),
            x = ~season,
            y = as.formula(paste0('~', input$y_var3)),
            type = "scatter",
            mode = "markers",
            color = ~playoffFlag,
            colors = c('#D37300', "#00126A"),
            size = 10
        ) %>%
            add_trace(data = line.data_reactive(),
                      x = ~season,
                      y = as.formula(paste0('~', input$y_var3)),
                      type = "scatter",
                      mode = 'lines', color=I('black'),
                      line = list(width = 2),
                      opacity = 0.35,
                      showlegend = F) %>%
            layout(title = paste0('', input$team, ' ', input$y_var3, ' Over the Last 5 Seasons'), font = t)
    })
    
    
    
    
    # Create playoff graph 1
    output$playoffPlot1 <- renderPlotly({
        plot_ly(
            data = all.data.playoff_reactive(),
            x = ~team,
            y = as.formula(paste0('~', input$y_var4)),
            type = "bar",
            color = ~distance,
            colors = c('#FF0000', "#FFA600", "#0A720F", "#000000", "#0045D8", "#CA0071"),
            size = 3
        ) %>%
            layout(title = paste0('Seasonal Playoff ', input$y_var4, ' Compared to NHL Average from 2017 to 2022'), font = t,
                   shapes = list(hline(mean(historic.avg.playoff_reactive()[,var.names == input$y_var4]), "red")),
                   xaxis = list(categoryorder = "total ascending"))
    })
    
    
    # playoff plot 2
    output$playoffPlot2 <- renderPlotly({
        plot_ly(
            data = all.data.playoff_reactive(),
            x = as.formula(paste0('~', input$x_var5)),
            y = as.formula(paste0('~', input$y_var5)),
            type = "scatter",
            mode = "markers",
            color = ~distance,
            colors = c('#FF0000', "#FFA600", "#0A720F", "#000000", "#0045D8", "#CA0071"),
            symbol = ~division,
            size = 3
        )%>%
            layout(title = paste0('NHL Playoff Stats: ', input$x_var5, " vs. ", input$y_var5), font = t)
    })
    
}





# Run the application 
shinyApp(ui = ui, server = server)








# https://www.moneypuck.com/teams.htm
# https://www.moneypuck.com/data.htm