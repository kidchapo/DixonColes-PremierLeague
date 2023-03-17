library(tidyverse)
library(worldfootballR)
library(regista)
library(goalmodel)

### Get Premier League results for the last 2 years via the worldfootballR package
datalist = list()
for (i in 2022:2023) {
  
  dat <- understat_league_match_results(league = "EPL", season_start_year = i)
  dat$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- dat # add it to your list
}


bigdata <- do.call(rbind, datalist)
sum(bigdata$home_goals,bigdata$away_goals)


### Select only the desired columns (Home, Away, HomeGoals, AwayGoals, etc)
bigepl <- bigdata[, c(15,6,9,11,12,13,14)]
bigepl$datetime <- as.Date(bigepl$datetime)

### Use datetime to assign larger weight for the most recent results
my_weights <- weights_dc(bigepl$datetime, xi=0.0019)

length(my_weights)


### Fitting the goalmodel of the goalmodel package, utilizing the Dixon-Coles Paper
dixcoles <- goalmodel(goals1 = bigepl$home_goals, goals2 = bigepl$away_goals,
                      team1 = bigepl$home_team, team2=bigepl$away_team, dc=TRUE,weights = my_weights)

summary(dixcoles)


### Fetch this years results 
matches <- fb_match_results(
  country = 'ENG',
  gender = 'M',
  season_end_year = 2023,
  tier = "1st",
  non_dom_league_url = NA
)

dixcoles$all_teams == sort(unique(matches$Home))

sort(unique(matches$Home))

### Rename teams accordingly
matches['Home'][matches['Home']=='Leicester City'] <- 'Leicester'
matches['Home'][matches['Home']=='Leeds United'] <- 'Leeds'
matches['Home'][matches['Home']=='Manchester Utd'] <- 'Manchester United'
matches['Home'][matches['Home']=='Newcastle Utd'] <- 'Newcastle United'
matches['Home'][matches['Home']=="Nott'ham Forest"] <- 'Nottingham Forest'
matches['Home'][matches['Home']=="Wolves"] <- 'Wolverhampton Wanderers'

matches['Away'][matches['Away']=='Leicester City'] <- 'Leicester'
matches['Away'][matches['Away']=='Leeds United'] <- 'Leeds'
matches['Away'][matches['Away']=='Manchester Utd'] <- 'Manchester United'
matches['Away'][matches['Away']=='Newcastle Utd'] <- 'Newcastle United'
matches['Away'][matches['Away']=="Nott'ham Forest"] <- 'Nottingham Forest'
matches['Away'][matches['Away']=="Wolves"] <- 'Wolverhampton Wanderers'

### Get the matches that are going to be played today. You can change the date accordingly
matches <- matches %>%
  filter(Date == Sys.Date())

to_predict1 <- list(matches$Home)[[1]]
to_predict2 <- list(matches$Away)[[1]]

### Over dataframe will hold the expected goals for each team for the match(/es)
over <- predict_expg(dixcoles, team1=to_predict1, team2=to_predict2, return_df = TRUE)
over <- over %>% mutate(across(where(is.numeric), round, 2))

over$sum <- rowSums( over[,3:4] )
over

### Win dataframe will hold the probabilities of win/draw/lose for the game at hand
win <- predict_result(dixcoles, team1=to_predict1, team2=to_predict2, return_df = TRUE)
round(win[,c(3:5)],2)

win <- win %>%
  mutate(across(where(is.numeric), round, 2)) 
win