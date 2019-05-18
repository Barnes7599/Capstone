library(tidyverse)

#Reading in dataset
batting <- read_csv("Batting.csv")
salary <- read_csv("Salaries.csv")

#Checking the str of data set
head(batting)
str(batting)
head(batting$AB, 5)
head(batting$'2B')
tail(batting$BA,5)

#Creating Batting Average column
batting$BA <- batting$H / batting$AB

#Creating On Base Percent column
batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)

#Creating column for singles 
batting$X1B <- batting$H - batting$`2B` - batting$`3B` - batting$HR

#Creating Slugging Percent column
batting$SLG <- ((1 * batting$X1B) + (2 * batting$`2B`) + (3 * batting$`3B`) + (4 * batting$HR)) / batting$AB

#Verifing sturcture of data set after adding new columns
str(batting)

#filtering out data to include only observations on or after 1985
batting <- batting %>% 
    filter(yearID >= 1985)
    # or subset(batting, yearID >= 1985)

#Verifing filter of yearID above
summary(batting)

#Joining salary and batting tables
combo <- inner_join(batting, salary, by = c("playerID", "yearID"))
    # or merge(batting, salary, by = c('playerID', 'yearID'))

#Verifing proper join
summary(combo)

#filtering to view onlt the lost players to understand what we need to replace
lost_players <- combo %>% 
    filter(playerID %in% c('damonjo01', 'giambja01', 'saenzol01'), yearID == 2001)
    # or...subset(combo, playerID %in% c('damonjo01', 'giambja01', 'saenzol01')) 
    #and...lost_players <- subset(lost_players, yearID == 2001)

#Verifing the lost_players filter
head(lost_players)

#Filtering for available players with constraints (salary < 8M, OBP >= 500, year == 2001(year A's lost thier star players (lost_players)))
available_players <- combo %>% 
    filter(salary < 8000000, OBP > 0, AB >= 500, yearID == 2001)

#visualizing the available_players on a simple scatterplot
ggplot(available_players, aes(OBP, salary)) +
    geom_point()

#filtering for possible replacements and excluding Jason Giambi since he met the filter constraints but know he is not going to sign with the A's
possible_replacements <- available_players %>% 
    arrange(desc(OBP)) %>% 
    select(playerID, OBP, AB, salary) %>% 
    filter(playerID != "giambja01")

#Viewing possible_replacements
possible_replacements
