library(tidyverse)

batting <- read_csv("Batting.csv")
salary <- read_csv("Salaries.csv")

head(batting)
str(batting)
head(batting$AB, 5)
head(batting$'2B')
batting$BA <- batting$H / batting$AB

tail(batting$BA,5)

batting$OBP <- (batting$H + batting$BB + batting$HBP) / (batting$AB + batting$BB + batting$HBP + batting$SF)

head(batting$OBP)

batting$X1B <- batting$H - batting$`2B` - batting$`3B` - batting$HR

batting$SLG <- ((1 * batting$X1B) + (2 * batting$`2B`) + (3 * batting$`3B`) + (4 * batting$HR)) / batting$AB

str(batting)

batting <- subset(batting, yearID >= 1985)
summary(batting)

combo <- merge(batting, salary, by = c('playerID', 'yearID'))

summary(combo)

lost_players <- subset(combo, playerID %in% c('damonjo01', 'giambja01', 'saenzol01')) 

lost_players <- subset(lost_players, yearID == 2001)

head(lost_players)

ggplot(available_players, aes(OBP, salary)) +
    geom_point()

available_players <- combo %>% 
    filter(salary < 8000000, OBP > 0, AB >= 500, yearID == 2001)

possible_replacements <- available_players %>% 
    arrange(desc(OBP)) %>% 
    select(playerID, OBP, AB, salary)

possible_replacements
