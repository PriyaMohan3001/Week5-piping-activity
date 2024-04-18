#Install and call the dplyr package. 
#install.packages("dplyr") # uncomment this if 'dplyr' package is not installed already
library(dplyr)

#Download to get and save the dataset
download.file(url = "https://projects.fivethirtyeight.com/soccer-api/international/2022/wc_matches.csv", destfile = "WorldCup.csv")
#Name your dataset
WorldCup<- read.csv("WorldCup.csv")
 
# Print the first few rows of the dataset
head(WorldCup)
# Check the structure of the dataset
str(WorldCup)
#To get the dimensions- number of rows and colums in a dataset
dim(WorldCup)
#To view the dataset
View(WorldCup)
# Summarize the dataset
summary(WorldCup)

#Let's make a random sample of our data and save it
mysample<-sample_n(WorldCup, size=15, replace = FALSE, weight = NULL, .env = NULL)

#Save the new sample as a csv file
write.csv(mysample, 'sample.csv')
sample_csv <- read.csv('sample.csv')

#Example: Let's try some piping with our mysample data. Note how the dataset name is not repeated in each function
piping <- mysample %>% 
  rename(SoccerPowerIndex = spi1) %>%
  subset(SoccerPowerIndex >60) %>%
  dim()%>%
  print()

#Executing some functions using piping
mysample2 <- mysample %>%
  arrange(date) %>%
  filter(spi1 < 80) %>%
  rename(Index1 = spi1, Index2 = spi2) %>%
  select(Index1, Index2, team1, team2) %>%
  summary() %>%
  print()

# Team stats
teamstats <- WorldCup %>%
            mutate(team = team1, performance_index = spi1) %>%
            bind_rows(WorldCup %>% mutate(team = team2, performance_index = spi2)) %>%
            group_by(team) %>%
            summarise(soccerperformanceindex = sum(performance_index),matches_played = n()) %>%
            arrange(soccerperformanceindex )

#Visualise performance index of teams
library(ggplot2)
ggplot(teamstats, aes(x=reorder(team, soccerperformanceindex), y=soccerperformanceindex)) + 
  geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(title = "performance index of teams", x = "team", y="soccerperformanceindex")
