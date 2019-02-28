library(dplyr)
library(lubridate)

setwd("~/Cricket_Hackathon/code")

australian_bowler_stats <- read.csv(file = 'australia_bowler_stats.csv',stringsAsFactors = F)
australian_team_stats <- read.csv(file = 'australia_team_stats.csv',stringsAsFactors = F)
australian_bowler_stats$X <- NULL
australian_team_stats$X <- NULL
australian_bowler_stats$teamName <- 'Australia'

australian_team_stats$date <- as.Date(australian_team_stats$date)

# for matches where player dnb, replace - with 0 
australian_bowler_stats$player_name <- ifelse(australian_bowler_stats$player_name	=='-',0,	australian_bowler_stats$player_name)
australian_bowler_stats$teamName <- ifelse(australian_bowler_stats$teamName=='-',0,australian_bowler_stats$teamName)
australian_bowler_stats$overs <- ifelse(australian_bowler_stats$overs=='-',0,australian_bowler_stats$overs)
australian_bowler_stats$maidens <- ifelse(australian_bowler_stats$maidens=='-',0,australian_bowler_stats$maidens)
australian_bowler_stats$runs_given <- ifelse(australian_bowler_stats$runs_given=='-',0,australian_bowler_stats$runs_given)
australian_bowler_stats$wickets <- ifelse(australian_bowler_stats$wickets=='-',0,australian_bowler_stats$wickets)
australian_bowler_stats$economy <- ifelse(australian_bowler_stats$economy=='-',0,australian_bowler_stats$economy)
australian_bowler_stats$innings_number <- ifelse(australian_bowler_stats$innings_number=='-',0,australian_bowler_stats$innings_number)
australian_bowler_stats$opposition <- ifelse(australian_bowler_stats$opposition=='-',0,australian_bowler_stats$opposition)
australian_bowler_stats$match_format <- ifelse(australian_bowler_stats$match_format=='-',0,australian_bowler_stats$match_format)
australian_bowler_stats$venue <- ifelse(australian_bowler_stats$venue=='-',0,australian_bowler_stats$venue)
australian_bowler_stats$date <- ifelse(australian_bowler_stats$date=='-',0,australian_bowler_stats$date)
australian_bowler_stats$city <- NULL
australian_bowler_stats$batting_position <- NULL


# convert everything to non-factors
australian_bowler_stats$player_name	     <- as.character(australian_bowler_stats$player_name)
australian_bowler_stats$teamName          <- as.character(australian_bowler_stats$teamName)
australian_bowler_stats$overs              <- as.integer(australian_bowler_stats$overs)
australian_bowler_stats$maidens       <- as.integer(australian_bowler_stats$maidens)
australian_bowler_stats$runs_given               <- as.integer(australian_bowler_stats$runs_given)
australian_bowler_stats$wickets               <- as.integer(australian_bowler_stats$wickets)
australian_bowler_stats$economy       <- as.numeric(australian_bowler_stats$economy)
australian_bowler_stats$innings_number    <- as.integer(australian_bowler_stats$innings_number)
australian_bowler_stats$opposition        <- as.character(australian_bowler_stats$opposition)
australian_bowler_stats$match_format      <- as.character(australian_bowler_stats$match_format)
australian_bowler_stats$venue             <- as.character(australian_bowler_stats$venue)
australian_bowler_stats$date              <- as.Date(australian_bowler_stats$date)

australian_bowler_stats$overs <- ifelse(is.na(australian_bowler_stats$overs),0,australian_bowler_stats$overs)

australia_squad <- c(  'AJ Finch',  'JP Behrendorff',  'AT Carey',  'NM Coulter-Nile',  'PJ Cummins',  'PSP Handscomb',  
                       'UT Khawaja',  'NM Lyon', 'SE Marsh',  'GJ Maxwell',  'KW Richardson',  'JA Richardson',  
                       'MP Stoinis',  'AJ Turner',  'A Zampa',  'DJM Short')

# lets get the stats for these players only
australian_squad_stats <- australian_bowler_stats %>% filter(player_name %in% australia_squad) %>% arrange(date,innings_number)
australian_squad_odi_stats <- australian_squad_stats %>% filter(match_format == 'ODI')

# under city, note down which playing country is the venue in. For eg - harare is in 'ZIMBABWE'

# now, lets prepare player wise stats
# no of "Test"  "ODI"  "T20I" played before this match in last year that the team played
# so if the player has played in 4 out of the 15 matches played in last 1 year, ratio is 4/15

for( match_number in 1:nrow(australian_squad_odi_stats)){
  print(match_number)
  
  match_stat <- australian_squad_odi_stats[match_number,]
  player_stats <- australian_squad_odi_stats %>% filter(player_name == match_stat$player_name)
  
  date_one_year_back <- match_stat$date %m-% months(12)
  
  total_wickets_taken <- player_stats %>% filter(date < match_stat$date) %>% select(wickets)
  if(nrow(total_wickets_taken) == 0){
    total_wickets_taken <- 0
  }else{
    total_wickets_taken <- sum(total_wickets_taken)
  }
  
  total_overs_bowled <- player_stats %>% filter(date < match_stat$date) %>% select(overs)
  if(nrow(total_overs_bowled) == 0){
    total_overs_bowled <- 0
  }else{
    total_overs_bowled <- sum(total_overs_bowled)
  }
  
  total_maidens_bowled <- player_stats %>% filter(date < match_stat$date) %>% select(maidens)
  if(nrow(total_maidens_bowled) == 0){
    total_maidens_bowled <- 0
  }else{
    total_maidens_bowled <- sum(total_maidens_bowled)
  }
  
  total_runs_given <- player_stats %>% filter(date < match_stat$date) %>% select(runs_given)
  if(nrow(total_runs_given) == 0){
    total_runs_given <- 0
  }else{
    total_runs_given <- sum(total_runs_given)
  }
  
  matches_played_by_australia_last_year <- australian_team_stats %>% filter(date < match_stat$date 
                                                                            & date > date_one_year_back 
                                                                            & match_format == match_stat$match_format) %>% nrow()
  last_year_player_stats <- player_stats %>% filter(date < match_stat$date & date > date_one_year_back)
  matches_played_last_year <- nrow(last_year_player_stats)
  
  wickets_taken_last_year <- sum(last_year_player_stats$wickets)
  
  if(matches_played_last_year == 0)
    avg_wickets_last_year <- 0
  else
    avg_wickets_last_year <- wickets_taken_last_year/matches_played_last_year
  
  ratio_of_matches_played_last_year <- matches_played_last_year / matches_played_by_australia_last_year
  
  australian_squad_odi_stats[match_number,"ratio_of_matches_played_last_year"] <- ratio_of_matches_played_last_year
  australian_squad_odi_stats[match_number,"total_wickets_taken"] <- total_wickets_taken
  australian_squad_odi_stats[match_number,"total_overs_bowled"] <- total_overs_bowled
  australian_squad_odi_stats[match_number,"total_maidens_bowled"] <- total_maidens_bowled
  australian_squad_odi_stats[match_number,"total_runs_given"] <- total_runs_given
  australian_squad_odi_stats[match_number,"wickets_taken_last_year"] <- wickets_taken_last_year
  australian_squad_odi_stats[match_number,"avg_wickets_last_year"] <- avg_wickets_last_year
  
}

# 463 rows, 19 cols

venue_details <- read.csv('venue_to_hometeam_mapping.csv',stringsAsFactors = F, header = F, strip.white = T)
colnames(venue_details) <- c('venue','home_country')
venue_details <- venue_details %>% unique()

australian_squad_odi_stats <- merge(x=australian_squad_odi_stats,y=venue_details,by = 'venue') %>% arrange(date) %>% unique()
australian_squad_odi_stats$is_home_venue <- ifelse(australian_squad_odi_stats$teamName == australian_squad_odi_stats$home_country,1,0)
australian_squad_odi_stats$is_opposition_home_venue <- ifelse(australian_squad_odi_stats$opposition == australian_squad_odi_stats$home_country,1,0)

# build models for every player

# we will consider only batsmen for this analysis
player_name <- c(  'AJ Finch','NM Lyon','GJ Maxwell','KW Richardson','JA Richardson','A Zampa')
wickets_prediction <- as.data.frame(matrix(nrow = length(player_name),ncol = 5))
colnames(wickets_prediction) <- c('player_name','lm_rmse','rf_rmse','gbrf_rmse','predicted_wickets')

i <- 1
options(warn=-1)
for(player in player_name){
  
  print(player)
  df <- australian_squad_odi_stats %>% filter(player_name == player)
  player_latest_stats <- df %>% arrange(desc(date)) %>% head(1)
  
  # lets prepare the 5 odi dataset to predict on
  test_odi_df <- data.frame(matrix(nrow = 1,ncol = 11))
  colnames(test_odi_df) <- c('wickets','opposition','ratio_of_matches_played_last_year','total_wickets_taken',
                             'total_overs_bowled','total_maidens_bowled','total_runs_given','wickets_taken_last_year',
                             'avg_wickets_last_year','is_home_venue','is_opposition_home_venue')
  test_odi_df$opposition <- 'India'
  test_odi_df$is_home_venue <- 1
  test_odi_df$is_opposition_home_venue <- 0
  test_odi_df$wickets <- 0
  test_odi_df$ratio_of_matches_played_last_year <- player_latest_stats$ratio_of_matches_played_last_year
  test_odi_df$total_wickets_taken <- player_latest_stats$total_wickets_taken + player_latest_stats$wickets
  test_odi_df$total_overs_bowled <- player_latest_stats$total_overs_bowled + player_latest_stats$overs
  test_odi_df$total_maidens_bowled <- player_latest_stats$total_maidens_bowled + player_latest_stats$maidens
  test_odi_df$total_runs_given <- player_latest_stats$total_runs_given  + player_latest_stats$runs_given
  test_odi_df$wickets_taken_last_year <- player_latest_stats$wickets_taken_last_year
  test_odi_df$avg_wickets_last_year <- player_latest_stats$avg_wickets_last_year
  test_odi_df$opposition <- as.factor(test_odi_df$opposition)
  
  df$runs_given <- NULL
  df$venue <- NULL
  df$player_name <- NULL
  df$teamName <- NULL
  df$overs <- NULL
  df$maidens <- NULL
  df$runs_given <- NULL
  df$economy <- NULL              
  df$innings_number <- NULL
  df$match_format <- NULL           
  df$date <- NULL
  df$home_country <- NULL
  df$opposition <- as.factor(df$opposition)
  
  levels(test_odi_df$opposition) <- levels(df$opposition)
  
  training_row_nos <- floor(0.8*nrow(df))
  test_row_nos <- training_row_nos + 1
  train_df <- df[1:training_row_nos,]
  test_df <- df[test_row_nos:nrow(df),]
  
  sapply(train_df, function(x) length(which(is.na(x))))
  
  set.seed(101)
  
  #### Model 1: Linear regression  
  model_lm <- lm(wickets ~ ratio_of_matches_played_last_year + total_wickets_taken + 
                   total_overs_bowled + total_maidens_bowled + total_runs_given + wickets_taken_last_year + 
                   avg_wickets_last_year + is_home_venue + is_opposition_home_venue ,data= train_df)
  pred <- predict(model_lm, test_df) %>% round(digits = 0)
  cbind(test_df$wickets,pred)
  RMSE(test_df$wickets,pred)
  wickets_prediction$lm_rmse[i] <- RMSE(test_df$wickets,pred)
  lm_wickets <- sum(pred)
  
  #### Model 2: Random Forest
  library(randomForest)
  model_rf <- randomForest(wickets~ opposition + ratio_of_matches_played_last_year + total_wickets_taken + 
                             total_overs_bowled + total_maidens_bowled + total_runs_given + wickets_taken_last_year + 
                             avg_wickets_last_year + is_home_venue + is_opposition_home_venue 
                           ,ntree = 500, data= train_df, mtry=10)
  predicted_wickets <- predict(model_rf, test_df) %>% round(digits = 0)
  cbind(test_df$wickets,predicted_wickets)
  RMSE(test_df$wickets,predicted_wickets)
  wickets_prediction$rf_rmse[i] <- RMSE(test_df$wickets,predicted_wickets)
  rf_wickets <- sum(predicted_wickets)
  
  #### Model 3: XGBoost 
  library(xgboost)
  library(Matrix)
  
  trainm<- sparse.model.matrix(wickets~., data = train_df)
  train_label <- train_df$wickets
  train_Data <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)
  
  testm<- sparse.model.matrix(wickets~., data = test_df)
  test_label <- test_df$wickets
  test_Data <- xgb.DMatrix(data = as.matrix(testm), label = test_label)
  
  bst_model <- xgb.train(nrounds = 100, data = train_Data)
  bst_model
  
  predicted_wickets_xg <- predict(bst_model, newdata = test_Data) %>% round(0)
  cbind(test_df$wickets, predicted_wickets_xg)
  RMSE(test_df$wickets,predicted_wickets_xg)
  wickets_prediction$gbrf_rmse[i] <- RMSE(test_df$wickets,predicted_wickets_xg)
  wickets_prediction$player_name[i] <- player
  bst_wickets <- sum(predicted_wickets_xg)
  
  # choose the best model based on rmse
  best_rmse <- min(wickets_prediction$gbrf_rmse[i],wickets_prediction$rf_rmse[i],wickets_prediction$lm_rmse[i])
  if(best_rmse == wickets_prediction$gbrf_rmse[i]){
    testm<- sparse.model.matrix(wickets~., data = test_odi_df)
    test_label <- test_odi_df$wickets
    test_Data <- xgb.DMatrix(data = as.matrix(testm), label = test_label)
    prediction <- predict(bst_model, newdata = test_Data) %>% round(0)
    wickets_prediction$predicted_wickets[i] <- sum(prediction)
  }else{
    if(best_rmse == wickets_prediction$rf_rmse[i]){
      prediction <- predict(model_rf, newdata = test_odi_df) %>% round(0)
      wickets_prediction$predicted_wickets[i] <- sum(prediction)
    }else{
      prediction <- predict(model_lm, newdata = test_odi_df) %>% round(0)
      wickets_prediction$predicted_wickets[i] <- sum(prediction)
    }
  }
  
  # cv_results <- xgb.cv(nrounds = 100, data = train_Data, nfold = 5)
  # cv_results
  i <- i + 1
  print('done')
}

options(warn=0)

# the run predictions are as follows --
wickets_prediction

#   player_name   lm_rmse   rf_rmse gbrf_rmse predicted_wickets
#      AJ Finch 0.0000000 0.0000000 0.3872983                 0
#       NM Lyon 2.3452079 0.8660254 1.0000000                 1
#    GJ Maxwell 0.5270463 0.7071068 0.2357023                 0
# KW Richardson 1.5811388 0.8660254 1.9364917                 2
# JA Richardson 2.0000000 1.5811388 2.2360680                 3
#       A Zampa 4.8989795 1.7728105 2.2990681                 2

write.csv(wickets_prediction,'australia_wickets_prediction')











