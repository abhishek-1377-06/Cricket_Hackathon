library(dplyr)
library(lubridate)

setwd("~/Cricket_Hackathon/code")

indian_batsman_stats <- read.csv(file = 'india_batsman_stats.csv',stringsAsFactors = F)
indian_team_stats <- read.csv(file = 'india_team_stats.csv',stringsAsFactors = F)
indian_batsman_stats$X <- NULL
indian_team_stats$X <- NULL
indian_batsman_stats$teamName <- 'India'

indian_team_stats$date <- as.Date(indian_team_stats$date)

# for matches where player dnb, replace - with 0 
indian_batsman_stats$player_name <- ifelse(indian_batsman_stats$player_name	=='-',0,	indian_batsman_stats$player_name)
indian_batsman_stats$teamName <- ifelse(indian_batsman_stats$teamName=='-',0,indian_batsman_stats$teamName)
indian_batsman_stats$runs <- ifelse(indian_batsman_stats$runs=='-',0,indian_batsman_stats$runs)
indian_batsman_stats$balls_faced <- ifelse(indian_batsman_stats$balls_faced=='-',0,indian_batsman_stats$balls_faced)
indian_batsman_stats$X4s <- ifelse(indian_batsman_stats$X4s=='-',0,indian_batsman_stats$X4s)
indian_batsman_stats$X6s <- ifelse(indian_batsman_stats$X6s=='-',0,indian_batsman_stats$X6s)
indian_batsman_stats$strike_rate <- ifelse(indian_batsman_stats$strike_rate=='-',0,indian_batsman_stats$strike_rate)
indian_batsman_stats$innings_number <- ifelse(indian_batsman_stats$innings_number=='-',0,indian_batsman_stats$innings_number)
indian_batsman_stats$opposition <- ifelse(indian_batsman_stats$opposition=='-',0,indian_batsman_stats$opposition)
indian_batsman_stats$match_format <- ifelse(indian_batsman_stats$match_format=='-',0,indian_batsman_stats$match_format)
indian_batsman_stats$venue <- ifelse(indian_batsman_stats$venue=='-',0,indian_batsman_stats$venue)
indian_batsman_stats$date <- ifelse(indian_batsman_stats$date=='-',0,indian_batsman_stats$date)
indian_batsman_stats$city <- ifelse(indian_batsman_stats$city=='-',0,indian_batsman_stats$city)
indian_batsman_stats$batting_position <- ifelse(indian_batsman_stats$batting_position=='-',0,indian_batsman_stats$batting_position)

# there are absent,DNB and - entries
# df <- cbind(indian_batsman_stats$runs,as.integer(indian_batsman_stats$runs)) %>% as.data.frame()
# indian_batsman_stats[which(is.na(df$V2)),]

# convert everything to non-factors
indian_batsman_stats$player_name	     <- as.character(indian_batsman_stats$player_name)
indian_batsman_stats$teamName          <- as.character(indian_batsman_stats$teamName)
indian_batsman_stats$runs              <- as.integer(indian_batsman_stats$runs)
indian_batsman_stats$balls_faced       <- as.integer(indian_batsman_stats$balls_faced)
indian_batsman_stats$X4s               <- as.integer(indian_batsman_stats$X4s)
indian_batsman_stats$X6s               <- as.integer(indian_batsman_stats$X6s)
indian_batsman_stats$strike_rate       <- as.numeric(indian_batsman_stats$strike_rate)
indian_batsman_stats$innings_number    <- as.integer(indian_batsman_stats$innings_number)
indian_batsman_stats$opposition        <- as.character(indian_batsman_stats$opposition)
indian_batsman_stats$match_format      <- as.character(indian_batsman_stats$match_format)
indian_batsman_stats$venue             <- as.character(indian_batsman_stats$venue)
indian_batsman_stats$date              <- as.Date(indian_batsman_stats$date)
indian_batsman_stats$city              <- as.character(indian_batsman_stats$city)
indian_batsman_stats$batting_position  <- as.integer(indian_batsman_stats$batting_position) 

indian_batsman_stats$runs <- ifelse(is.na(indian_batsman_stats$runs),0,indian_batsman_stats$runs)

# to get batting position, we will group by date and then assign rank value as batting_position
#indian_batsman_stats <- indian_batsman_stats %>% group_by(date,innings_number) %>% 
#  arrange(date,innings_number) %>% dplyr::mutate(rank = rank(player_name)) %>% arrange(date,innings_number,rank)

#indian_batsman_stats$batting_position <- indian_batsman_stats$rank
#indian_batsman_stats$rank <- NULL

india_squad <- c(  'V Kohli',  'RG Sharma',  'S Dhawan',  'AT Rayudu',  'KM Jadhav',  'MS Dhoni',  'JJ Bumrah',  'Mohammed Shami',
                   'YS Chahal',  'Kuldeep Yadav',  'V Shankar',  'RR Pant',  'S Kaul',  'KL Rahul',  'B Kumar',  'RA Jadeja')

# lets get the stats for these players only
indian_squad_stats <- indian_batsman_stats %>% filter(player_name %in% india_squad) %>% arrange(date,innings_number)
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
  
  total_runs_scored <- player_stats %>% filter(date < match_stat$date) %>% select(runs)
  if(nrow(total_runs_scored) == 0){
    total_runs_scored <- 0
  }else{
    total_runs_scored <- sum(total_runs_scored)
  }
  
  total_balls_faced <- player_stats %>% filter(date < match_stat$date) %>% select(balls_faced)
  
  if(nrow(total_balls_faced) == 0){
    total_balls_faced <- 0
  }
  
  #if(nrow(total_balls_faced) == 0 | (nrow(total_balls_faced) == 0 & sum(total_balls_faced) == 0)){
  #  total_balls_faced <- 1
  #}else{
  #  total_balls_faced <- sum(total_balls_faced)
  #}
  
  # strike_rate_till_now <- total_runs_scored/total_balls_faced
  #matches_at_this_venue <- player_stats %>% filter(venue == match_stat$venue) %>% select(runs)
  #no_of_matches_at_venue <- nrow(runs_at_this_venue)
  #runs_at_this_venue <- matches_at_this_venue %>% sum()
  #average_at_this_venue <- 
  
  matches_played_by_india_last_year <- indian_team_stats %>% filter(date < match_stat$date 
                                                                    & date > date_one_year_back 
                                                                    & match_format == match_stat$match_format) %>% nrow()
  last_year_player_stats <- player_stats %>% filter(date < match_stat$date & date > date_one_year_back)
  matches_played_last_year <- nrow(last_year_player_stats)
  
  runs_scored_last_year <- sum(last_year_player_stats$runs)
  
  if(matches_played_last_year == 0)
    avg_last_year <- 0
  else
    avg_last_year <- runs_scored_last_year/matches_played_last_year
  
  ratio_of_matches_played_last_year <- matches_played_last_year / matches_played_by_india_last_year
  centuries_last_year <- length(which(last_year_player_stats$runs > 100))
  half_centuries_last_year <- length(which(last_year_player_stats$runs > 50))
  
  indian_squad_odi_stats[match_number,"ratio_of_matches_played_last_year"] <- ratio_of_matches_played_last_year
  indian_squad_odi_stats[match_number,"runs_scored_last_year"] <- runs_scored_last_year
  indian_squad_odi_stats[match_number,"avg_last_year"] <- avg_last_year
  indian_squad_odi_stats[match_number,"centuries_last_year"] <- centuries_last_year
  indian_squad_odi_stats[match_number,"half_centuries_last_year"] <- half_centuries_last_year
  indian_squad_odi_stats[match_number,"month"] <- month(match_stat$date)
  indian_squad_odi_stats[match_number,"day"] <- day(match_stat$date)
  indian_squad_odi_stats[match_number,"total_runs_scored"] <- total_runs_scored
  indian_squad_odi_stats[match_number,"total_balls_faced"] <- total_balls_faced
  #indian_squad_odi_stats[match_number,"strike_rate_till_now"] <- strike_rate_till_now
  
}

# 1442 rows, 21 cols

venue_details <- read.csv('venue_to_hometeam_mapping.csv',stringsAsFactors = F, header = F, strip.white = T)
colnames(venue_details) <- c('venue','home_country')
venue_details <- venue_details %>% unique()

indian_squad_odi_stats <- merge(x=indian_squad_odi_stats,y=venue_details,by = 'venue') %>% arrange(date) %>% unique()
indian_squad_odi_stats$is_home_venue <- ifelse(indian_squad_odi_stats$teamName == indian_squad_odi_stats$home_country,1,0)
indian_squad_odi_stats$is_opposition_home_venue <- ifelse(indian_squad_odi_stats$opposition == indian_squad_odi_stats$home_country,1,0)

# build models for every player

# we will consider only batsmen for this analysis
player_name <- c("MS Dhoni","RG Sharma","V Kohli","RA Jadeja","S Dhawan","AT Rayudu","KL Rahul")
runs_prediction <- as.data.frame(matrix(nrow = length(player_name),ncol = 5))
colnames(runs_prediction) <- c('player_name','lm_rmse','rf_rmse','gbrf_rmse','predicted_runs')

i <- 1
options(warn=-1)
for(player in player_name){
  
  print(player)
  df <- indian_squad_odi_stats %>% filter(player_name == player)
  player_latest_stats <- df %>% arrange(desc(date)) %>% head(1)
  
  # lets prepare the 5 odi dataset to predict on
  test_odi_df <- data.frame(matrix(nrow = 1,ncol = 12))
  colnames(test_odi_df) <- c('runs','opposition','ratio_of_matches_played_last_year','runs_scored_last_year',
                             'avg_last_year','centuries_last_year','half_centuries_last_year','month',
                             'total_runs_scored','total_balls_faced','is_home_venue','is_opposition_home_venue')
  test_odi_df$opposition <- 'Australia'
  test_odi_df$is_home_venue <- 1
  test_odi_df$is_opposition_home_venue <- 0
  
  test_odi_df$ratio_of_matches_played_last_year <- player_latest_stats$ratio_of_matches_played_last_year
  test_odi_df$runs_scored_last_year <- player_latest_stats$runs_scored_last_year
  test_odi_df$avg_last_year <- player_latest_stats$avg_last_year
  test_odi_df$centuries_last_year <- player_latest_stats$centuries_last_year
  test_odi_df$half_centuries_last_year <- player_latest_stats$half_centuries_last_year
  test_odi_df$month <- 3
  test_odi_df$total_runs_scored <- player_latest_stats$total_runs_scored
  test_odi_df$total_balls_faced <- player_latest_stats$total_balls_faced
  test_odi_df$opposition <- as.factor(test_odi_df$opposition)
  
  df$X4s <- NULL
  df$balls_faced <- NULL
  df$X6s <- NULL
  df$strike_rate <- NULL
  df$innings_number <- NULL              
  df$date <- NULL           
  df$batting_position <- NULL
  df$day <- NULL
  df$home_country <- NULL
  df$teamName <- NULL
  df$venue <- NULL
  df$match_format <- NULL
  df$city <- NULL
  df$player_name <- NULL
  df$opposition <- as.factor(df$opposition)
  
  levels(test_odi_df$opposition) <- levels(df$opposition)
  
  training_row_nos <- floor(0.8*nrow(df))
  test_row_nos <- training_row_nos + 1
  train_df <- df[1:training_row_nos,]
  test_df <- df[test_row_nos:nrow(df),]
  
  sapply(train_df, function(x) length(which(is.na(x))))
  
  set.seed(101)
  
  #### Model 1: Linear regression  
  model_lm <- lm(runs~ ratio_of_matches_played_last_year + 
                   runs_scored_last_year + avg_last_year + centuries_last_year + half_centuries_last_year + 
                   month + is_home_venue + is_opposition_home_venue + total_runs_scored ,data= train_df)
  pred <- predict(model_lm, test_df) %>% round(digits = 0)
  cbind(test_df$runs,pred)
  RMSE(test_df$runs,pred)
  runs_prediction$lm_rmse[i] <- RMSE(test_df$runs,pred)
  lm_runs <- sum(pred)
  
  #### Model 2: Random Forest
  library(randomForest)
  model_rf <- randomForest(runs~ opposition + ratio_of_matches_played_last_year + 
                             runs_scored_last_year + avg_last_year + centuries_last_year + half_centuries_last_year + 
                             month + is_home_venue + is_opposition_home_venue + total_runs_scored 
                           ,ntree = 500, data= train_df, mtry=10)
  predicted_runs <- predict(model_rf, test_df) %>% round(digits = 0)
  cbind(test_df$runs,predicted_runs)
  RMSE(test_df$runs,predicted_runs)
  runs_prediction$rf_rmse[i] <- RMSE(test_df$runs,predicted_runs)
  rf_runs <- sum(predicted_runs)
  
  #### Model 3: XGBoost 
  library(xgboost)
  library(Matrix)
  
  trainm<- sparse.model.matrix(runs~., data = train_df)
  train_label <- train_df$runs
  train_Data <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)
  
  testm<- sparse.model.matrix(runs~., data = test_df)
  test_label <- test_df$runs
  test_Data <- xgb.DMatrix(data = as.matrix(testm), label = test_label)
  
  bst_model <- xgb.train(nrounds = 100, data = train_Data)
  bst_model
  
  predicted_runs_xg <- predict(bst_model, newdata = test_Data) %>% round(0)
  cbind(test_df$runs, predicted_runs_xg)
  RMSE(test_df$runs,predicted_runs_xg)
  runs_prediction$gbrf_rmse[i] <- RMSE(test_df$runs,predicted_runs_xg)
  runs_prediction$player_name[i] <- player
  bst_runs <- sum(predicted_runs_xg)
  
  # choose the best model based on rmse
  best_rmse <- min(runs_prediction$gbrf_rmse[i],runs_prediction$rf_rmse[i],runs_prediction$lm_rmse[i])
  if(best_rmse == runs_prediction$gbrf_rmse[i]){
    prediction <- predict(bst_model, newdata = test_odi_df) %>% round(0)
    runs_prediction$predicted_runs[i] <- sum(prediction)
  }else{
    if(best_rmse == runs_prediction$rf_rmse[i]){
      prediction <- predict(model_rf, newdata = test_odi_df) %>% round(0)
      runs_prediction$predicted_runs[i] <- sum(prediction)
    }else{
      prediction <- predict(model_lm, newdata = test_odi_df) %>% round(0)
      runs_prediction$predicted_runs[i] <- sum(prediction)
    }
  }
  
  # cv_results <- xgb.cv(nrounds = 100, data = train_Data, nfold = 5)
  # cv_results
  i <- i + 1
  print('done')
}

options(warn=0)

# the run predictions are as follows --
runs_prediction
write.csv(runs_prediction,'india_runs_prediction')

# player_name  lm_rmse  rf_rmse gbrf_rmse predicted_runs
#    MS Dhoni 38.99101 47.61177  89.01216             58
#   RG Sharma 62.37436 55.46302  61.39675             49
#     V Kohli 48.43300 49.28985  61.48514             54
#   RA Jadeja 24.44654 21.88835  30.24566             20
#    S Dhawan 64.33941 37.04916  47.89656             60
#   AT Rayudu 41.77211 37.95093  52.25984             36
#    KL Rahul 71.55883 20.19901  29.94996             26













