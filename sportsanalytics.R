rm(list=ls()); cat("\014") # clear all

library(SportsAnalytics)
library(googleVis)
library(XML)
library(stringr)
library(ggplot2)
library(tidyr)

#------------------------------------------------------------------------------------------------------------------------
# get 15-16 data -----------------------------------------
nba1516 <- fetch_NBAPlayerStatistics("15-16")
nba1516.gsw <- subset(nba1516, Team == "GSW")

# get 00-01 data -----------------------------------------
nba0001 <- fetch_NBAPlayerStatistics("00-01")
nba0001.gsw <- subset(nba0001, Team == "GSW")

# 00-01 standings -----------------------------------------
standings0001 <- read.csv("0001season.csv")
chart0001 <- gvisTable(standings0001[, c(1,2,3,4,8,9,10,11)])
plot(chart0001)

# 15-16 standings -----------------------------------------
standings1516 <- read.csv("1516season.csv")
chart1516 <- gvisTable(standings1516[, c(1,2,3,4,8,9,10,11)])
plot(chart1516)

# 0001 gsw roster -----------------------------------------
nba0001.gsw.roster <- gvisTable(nba0001.gsw[, c('Name', 'Position', 'GamesPlayed', 'TotalMinutesPlayed')])
plot(nba0001.gsw.roster)

# 1516 gsw roster -----------------------------------------
nba1516.gsw.roster <- gvisTable(nba1516.gsw[, c('Name', 'Position', 'GamesPlayed', 'TotalMinutesPlayed')])
plot(nba1516.gsw.roster)

# hist(nba0001.gsw$TotalMinutesPlayed, 
#      main= "Minutes played for 00/01 Warriors", 
#      breaks=seq(0, 3500, by = 500),
#      xlab = "Minutes Played", 
#      ylab = "Num Players",
#      ylim = c(0, 12))
# 
# hist(nba1516.gsw$TotalMinutesPlayed, 
#      main= "Minutes played for 15/16 Warriors",
#      breaks=seq(0, 3500, by = 500),
#      xlab = "Minutes Played", 
#      ylab = "Num Players",
#      ylim = c(0, 12))

# hist(nba0001.gsw$GamesPlayed, 
#      main= "Games played for 00-01 Warriors", 
#      breaks=seq(0, 100, by = 20),
#      xlab = "Games Played", 
#      ylab = "Num Players",
#      ylim = c(0, 12))
# 
# hist(nba1516.gsw$GamesPlayed, 
#      main= "Games played for 15-16 Warriors",
#      breaks=seq(0, 100, by = 20),
#      xlab = "Games Played", 
#      ylab = "Num Players",
#      ylim = c(0, 12))

# distribution of minutes played per player -----------------------------------------
hist1 <- gvisHistogram(nba0001.gsw[, c('Name', 'GamesPlayed')], 
                       options = list(histogram = "{bucketSize :20}", 
                                      hAxis = "{title: 'Players'}", 
                                      vAxis = "{title: 'Count', maxValue: 12}",
                                      title = 'Distribution of 00-01 GSW Games Played',
                                      legend = "{position: 'in'}",
                                      height = 300,
                                      width = 500))
plot(hist1)

hist2 <- gvisHistogram(nba1516.gsw[, c('Name', 'GamesPlayed')], 
                       options = list(histogram = "{bucketSize :20}", 
                                      hAxis = "{title: 'Players'}", 
                                      vAxis = "{title: 'Count', maxValue: 12}",
                                      title = 'Distribution of 15-16 GSW Games Played',
                                      legend = "{position: 'in'}",
                                      height = 300,
                                      width = 500))
plot(hist2)

# three point rankings -----------------------------------------
atleast10threes <- nba1516[nba1516$ThreesMade > 10,]
atleast10threes$ThreesPct <- atleast10threes$ThreesMade / atleast10threes$ThreesAttempted
atleast10threes$MinutesPerThree <- atleast10threes$TotalMinutesPlayed / atleast10threes$ThreesMade
threes <- atleast10threes[order(-atleast10threes$ThreesPct), 
                          c('Name', 
                            'Team', 
                            'ThreesMade', 
                            'ThreesAttempted', 
                            'ThreesPct', 
                            'MinutesPerThree')]
threes.chart <- gvisTable(head(threes, n = 5))
plot(threes.chart)

threes1 <- atleast10threes[order(-atleast10threes$ThreesMade), 
                           c('Name', 
                             'Team', 
                             'ThreesMade', 
                             'ThreesAttempted', 
                             'ThreesPct', 
                             'MinutesPerThree')]
threes1.chart <- gvisTable(head(threes1, n = 5))
plot(threes1.chart)

atleast10threes0001 <- nba0001[nba0001$ThreesMade > 10,]
atleast10threes0001$MinutesPerThree <- atleast10threes0001$TotalMinutesPlayed / atleast10threes0001$ThreesMade
atleast10threes0001$ThreesPct <- atleast10threes0001$ThreesMade / atleast10threes0001$ThreesAttempted
threes0001 <- atleast10threes0001[order(-atleast10threes0001$ThreesMade), 
                                  c('Name', 
                                    'Team', 
                                    'ThreesMade', 
                                    'ThreesAttempted', 
                                    'ThreesPct', 
                                    'MinutesPerThree')]
rownames(threes0001) <- NULL
threes0001$Rank <- seq.int(nrow(threes0001))
threes0001 <- threes0001[c('Rank', 
                           'Name', 
                           'Team', 
                           'ThreesMade', 
                           'ThreesAttempted', 
                           'ThreesPct', 
                           'MinutesPerThree')]
bestThreeShooter0001 <- gvisTable(threesmade0001 <- threes0001[threes0001$Team == 'GSW',])
plot(bestThreeShooter0001)


# hist(nba1516$ThreesMade, breaks = seq(0,420, by = 10))
# nba1516[nba1516$ThreesMade > 10,]

threesbyteam <- aggregate(list(Threes = nba1516$ThreesMade), 
                          by = list(Team=nba1516$Team), 
                          FUN = sum)
plot(gvisTable(head(threesbyteam[order(-threesbyteam$Threes),], n = 5)))

threesbyteam0001 <- aggregate(list(Threes = nba0001$ThreesMade), 
                              by = list(Team=nba0001$Team), 
                              FUN = sum)
plot(gvisTable(head(threesbyteam0001[order(threesbyteam0001$Threes),], n = 10)))


# time series of three pointers scored -----------------------------------------
df <- data.frame(Year = 'asdf',
                 GSW = double(16),
                 League = double(16), 
                 stringsAsFactors = FALSE) 

for (i in seq(1:16)) {
  tempYear <- paste(toString(sprintf('%02.0f', i-1)),
                    '-', 
                    toString(sprintf('%02.0f', i)), 
                    sep = '')
  tempData <- fetch_NBAPlayerStatistics(paste(toString(sprintf('%02.0f', i-1)),
                                              '-', 
                                              toString(sprintf('%02.0f', i)), 
                                              sep = ''))
  tempLeague <- mean(tempData$ThreesMade)
  tempGSW <- mean(tempData[tempData$Team == 'GSW',]$ThreesMade)
  df[i, 'Year'] <- tempYear
  df[i, 'GSW'] <- tempGSW
  df[i, 'League'] <- tempLeague
}

Line <- gvisLineChart(df, options = list(title = 'Average Number of Threes GSW vs. League',
                                         legend = "{position: 'in'}",
                                         height = 500,
                                         width = 900, 
                                         hAxis = "{title: 'Year'}", 
                                         vAxis = "{title: 'Mean Total Threes'}"))
plot(Line)


# time series of assists  -----------------------------------------
dfassist <- data.frame(Year = toString('asdf'),
                 GSW = double(16),
                 League = double(16), 
                 stringsAsFactors = FALSE) 

for (i in seq(1:16)) {
  tempYear <- paste(toString(sprintf('%02.0f', i-1)),
                    '-', 
                    toString(sprintf('%02.0f', i)), 
                    sep = '')
  tempData <- fetch_NBAPlayerStatistics(paste(toString(sprintf('%02.0f', i-1)),
                                              '-', 
                                              toString(sprintf('%02.0f', i)), 
                                              sep = ''))
  tempLeague <- mean(tempData$Assists)
  tempGSW <- mean(tempData[tempData$Team == 'GSW',]$Assists)
  dfassist[i, 'Year'] <- toString(tempYear)
  dfassist[i, 'GSW'] <- tempGSW
  dfassist[i, 'League'] <- tempLeague
}

LineAssists <- gvisLineChart(dfassist, options = list(title = 'Average Number Assists GSW vs. League', 
                                                      legend = "{position: 'in'}",
                                                      height = 500,
                                                      width = 900,
                                                      hAxis = "{title: 'Year'}",
                                                      vAxis = "{title: 'Mean Assists / Minute'}"))
plot(LineAssists)



# map of world cup winners  -----------------------------------------
webpage <- paste(readLines("BasketballWorldCupChampionsandMedalsbyYear.html"), collapse="\n")

data <- readHTMLTable(webpage,
                      which = 1,
                      stringsAsFactors = FALSE)

names(data) <- c("Input")

data[1, "Input"]
toDelete <- seq(2, nrow(data), 2)
data <-  data[-toDelete, ]
data <- data[,-2]
colnames(data) <- data[1,]
colnames(data)[1] <- 'World Cup'
data <- data[-1,]
rownames(data) <- NULL
data <- gather(data, Place, Country, Gold, Silver, Bronze)
data$Country[data$Country == 'USA'] <- 'United States'

geochart <- gvisGeoChart(data, locationvar = 'Country')

plot(geochart)





