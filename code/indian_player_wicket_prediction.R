library(dplyr)
library(lubridate)

setwd("~/Cricket_Hackathon/code")

indian_bowler_stats <- read.csv(file = 'india_bowler_stats.csv',stringsAsFactors = F)
indian_team_stats <- read.csv(file = 'india_team_stats.csv',stringsAsFactors = F)
indian_bowler_stats$X <- NULL
indian_team_stats$X <- NULL
indian_bowler_stats$teamName <- 'india'

indian_team_stats$date <- as.Date(indian_team_stats$date)

# for matches where player dnb, replace - with 0 
indian_bowler_stats$player_name <- ifelse(indian_bowler_stats$player_name	=='-',0,	indian_bowler_stats$player_name)
indian_bowler_stats$teamName <- ifelse(indian_bowler_stats$teamName=='-',0,indian_bowler_stats$teamName)
indian_bowler_stats$overs <- ifelse(indian_bowler_stats$overs=='-',0,indian_bowler_stats$overs)
indian_bowler_stats$maidens <- ifelse(indian_bowler_stats$maidens=='-',0,indian_bowler_stats$maidens)
indian_bowler_stats$runs_given <- ifelse(indian_bowler_stats$runs_given=='-',0,indian_bowler_stats$runs_given)
indian_bowler_stats$wickets <- ifelse(indian_bowler_stats$wickets=='-',0,indian_bowler_stats$wickets)
indian_bowler_stats$economy <- ifelse(indian_bowler_stats$economy=='-',0,indian_bowler_stats$economy)
indian_bowler_stats$innings_number <- ifelse(indian_bowler_stats$innings_number=='-',0,indian_bowler_stats$innings_number)
indian_bowler_stats$opposition <- ifelse(indian_bowler_stats$opposition=='-',0,indian_bowler_stats$opposition)
indian_bowler_stats$match_format <- ifelse(indian_bowler_stats$match_format=='-',0,indian_bowler_stats$match_format)
indian_bowler_stats$venue <- ifelse(indian_bowler_stats$venue=='-',0,indian_bowler_stats$venue)
indian_bowler_stats$date <- ifelse(indian_bowler_stats$date=='-',0,indian_bowler_stats$date)
indian_bowler_stats$city <- NULL
indian_bowler_stats$batting_position <- NULL


# convert everything to non-factors
indian_bowler_stats$player_name	     <- as.character(indian_bowler_stats$player_name)
indian_bowler_stats$teamName          <- as.character(indian_bowler_stats$teamName)
indian_bowler_stats$overs              <- as.integer(indian_bowler_stats$overs)
indian_bowler_stats$maidens       <- as.integer(indian_bowler_stats$maidens)
indian_bowler_stats$runs_given               <- as.integer(indian_bowler_stats$runs_given)
indian_bowler_stats$wickets               <- as.integer(indian_bowler_stats$wickets)
indian_bowler_stats$economy       <- as.numeric(indian_bowler_stats$economy)
indian_bowler_stats$innings_number    <- as.integer(indian_bowler_stats$innings_number)
indian_bowler_stats$opposition        <- as.character(indian_bowler_stats$opposition)
indian_bowler_stats$match_format      <- as.character(indian_bowler_stats$match_format)
indian_bowler_stats$venue             <- as.character(indian_bowler_stats$venue)
indian_bowler_stats$date              <- as.Date(indian_bowler_stats$date)

indian_bowler_stats$overs <- ifelse(is.na(indian_bowler_stats$overs),0,indian_bowler_stats$overs)

india_squad <- c(  'V Kohli',  'RG Sharma',  'S Dhawan',  'AT Rayudu',  'KM Jadhav',  'MS Dhoni',  'JJ Bumrah',  'Mohammed Shami',
                   'YS Chahal',  'Kuldeep Yadav',  'V Shankar',  'RR Pant',  'S Kaul',  'KL Rahul',  'B Kumar',  'RA Jadeja')

# lets get the stats for these players only
indian_squad_stats <- indian_bowler_stats %>% filter(player_name %in% india_squad) %>% arrange(date,innings_number)
indian_squad_odi_stats <- indian_squad_stats %>% filter(match_format == 'ODI')

# under city, note down which playing country is the venue in. For eg - harare is in 'ZIMBABWE'

# now, lets prepare player wise stats
# no of "Test"  "ODI"  "T20I" played before this match in last year that the team played
# so if the player has played in 4 out of the 15 matches played in last 1 year, ratio is 4/15

for( match_number in 1:nrow(indian_squad_odi_stats)){
  print(match_number)
  
  match_stat <- indian_squad_odi_stats[match_number,]
  player_stats <- indian_squad_odi_stats %>% filter(player_name == match_stat$player_name)
  
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
  
  matches_played_by_india_last_year <- indian_team_stats %>% filter(date < match_stat$date 
                                                                    & date > date_one_year_back 
                                                                    & match_format == match_stat$match_format) %>% nrow()
  last_year_player_stats <- player_stats %>% filter(date < match_stat$date & date > date_one_year_back)
  matches_played_last_year <- nrow(last_year_player_stats)
  
  wickets_taken_last_year <- sum(last_year_player_stats$wickets)
  
  if(matches_played_last_year == 0)
    avg_wickets_last_year <- 0
  else
    avg_wickets_last_year <- wickets_taken_last_year/matches_played_last_year
  
  ratio_of_matches_played_last_year <- matches_played_last_year / matches_played_by_india_last_year
  
  indian_squad_odi_stats[match_number,"ratio_of_matches_played_last_year"] <- ratio_of_matches_played_last_year
  indian_squad_odi_stats[match_number,"total_wickets_taken"] <- total_wickets_taken
  indian_squad_odi_stats[match_number,"total_overs_bowled"] <- total_overs_bowled
  indian_squad_odi_stats[match_number,"total_maidens_bowled"] <- total_maidens_bowled
  indian_squad_odi_stats[match_number,"total_runs_given"] <- total_runs_given
  indian_squad_odi_stats[match_number,"wickets_taken_last_year"] <- wickets_taken_last_year
  indian_squad_odi_stats[match_number,"avg_wickets_last_year"] <- avg_wickets_last_year
  
}

# 1442 rows, 12 cols

venue_details <- read.csv('venue_to_hometeam_mapping.csv',stringsAsFactors = F, header = F, strip.white = T)
colnames(venue_details) <- c('venue','home_country')
venue_details <- venue_details %>% unique()

indian_squad_odi_stats <- merge(x=indian_squad_odi_stats,y=venue_details,by = 'venue') %>% arrange(date) %>% unique()
indian_squad_odi_stats$is_home_venue <- ifelse(indian_squad_odi_stats$teamName == indian_squad_odi_stats$home_country,1,0)
indian_squad_odi_stats$is_opposition_home_venue <- ifelse(indian_squad_odi_stats$opposition == indian_squad_odi_stats$home_country,1,0)

# build models for every player

# we will consider only batsmen for this analysis
player_name <- c('KM Jadhav','JJ Bumrah','Mohammed Shami','YS Chahal','Kuldeep Yadav','V Shankar','S Kaul','B Kumar','RA Jadeja')
wickets_prediction <- as.data.frame(matrix(nrow = length(player_name),ncol = 5))
colnames(wickets_prediction) <- c('player_name','lm_rmse','rf_rmse','gbrf_rmse','predicted_wickets')

i <- 1
options(warn=-1)
for(player in player_name){
  
  print(player)
  df <- indian_squad_odi_stats %>% filter(player_name == player)
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

#    player_name   lm_rmse  rf_rmse gbrf_rmse predicted_wickets
#      KM Jadhav 0.8528029 0.797724  1.381699                 1
#      JJ Bumrah 2.8480012 1.154701  1.632993                 2
# Mohammed Shami 4.8476799 1.224745  1.500000                 2
#      YS Chahal 1.4577380 1.802776  1.457738                 1
#  Kuldeep Yadav 2.3184046 1.457738  1.414214                 2
#      V Shankar 0.0000000 0.000000  0.000000                 0
#         S Kaul 0.0000000 0.000000  0.000000                 0
#        B Kumar 1.2909944 1.253566  2.093072                 1
#      RA Jadeja 1.9148542 1.140175  1.169045                 1

write.csv(wickets_prediction,'india_wickets_prediction')











