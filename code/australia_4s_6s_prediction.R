library(dplyr)
library(lubridate)

setwd("~/Cricket_Hackathon/code")

australian_batsman_stats <- read.csv(file = 'australia_batsman_stats.csv',stringsAsFactors = F)
australian_team_stats <- read.csv(file = 'australia_team_stats.csv',stringsAsFactors = F)
australia_runs_prediction <- read.csv('australia_runs_prediction',stringsAsFactors = F)
australian_batsman_stats$X <- NULL
australian_team_stats$X <- NULL
australian_batsman_stats$teamName <- 'australia'

australian_team_stats$date <- as.Date(australian_team_stats$date)

# for matches where player dnb, replace - with 0 
australian_batsman_stats$player_name <- ifelse(australian_batsman_stats$player_name	=='-',0,	australian_batsman_stats$player_name)
australian_batsman_stats$teamName <- ifelse(australian_batsman_stats$teamName=='-',0,australian_batsman_stats$teamName)
australian_batsman_stats$runs <- ifelse(australian_batsman_stats$runs=='-',0,australian_batsman_stats$runs)
australian_batsman_stats$balls_faced <- ifelse(australian_batsman_stats$balls_faced=='-',0,australian_batsman_stats$balls_faced)
australian_batsman_stats$X4s <- ifelse(australian_batsman_stats$X4s=='-',0,australian_batsman_stats$X4s)
australian_batsman_stats$X6s <- ifelse(australian_batsman_stats$X6s=='-',0,australian_batsman_stats$X6s)
australian_batsman_stats$strike_rate <- ifelse(australian_batsman_stats$strike_rate=='-',0,australian_batsman_stats$strike_rate)
australian_batsman_stats$innings_number <- ifelse(australian_batsman_stats$innings_number=='-',0,australian_batsman_stats$innings_number)
australian_batsman_stats$opposition <- ifelse(australian_batsman_stats$opposition=='-',0,australian_batsman_stats$opposition)
australian_batsman_stats$match_format <- ifelse(australian_batsman_stats$match_format=='-',0,australian_batsman_stats$match_format)
australian_batsman_stats$venue <- ifelse(australian_batsman_stats$venue=='-',0,australian_batsman_stats$venue)
australian_batsman_stats$date <- ifelse(australian_batsman_stats$date=='-',0,australian_batsman_stats$date)
australian_batsman_stats$city <- ifelse(australian_batsman_stats$city=='-',0,australian_batsman_stats$city)
australian_batsman_stats$batting_position <- ifelse(australian_batsman_stats$batting_position=='-',0,australian_batsman_stats$batting_position)

# there are absent,DNB and - entries
# df <- cbind(australian_batsman_stats$runs,as.integer(australian_batsman_stats$runs)) %>% as.data.frame()
# australian_batsman_stats[which(is.na(df$V2)),]

# convert everything to non-factors
australian_batsman_stats$player_name	     <- as.character(australian_batsman_stats$player_name)
australian_batsman_stats$teamName          <- as.character(australian_batsman_stats$teamName)
australian_batsman_stats$runs              <- as.integer(australian_batsman_stats$runs)
australian_batsman_stats$balls_faced       <- as.integer(australian_batsman_stats$balls_faced)
australian_batsman_stats$X4s               <- as.integer(australian_batsman_stats$X4s)
australian_batsman_stats$X6s               <- as.integer(australian_batsman_stats$X6s)
australian_batsman_stats$strike_rate       <- as.numeric(australian_batsman_stats$strike_rate)
australian_batsman_stats$innings_number    <- as.integer(australian_batsman_stats$innings_number)
australian_batsman_stats$opposition        <- as.character(australian_batsman_stats$opposition)
australian_batsman_stats$match_format      <- as.character(australian_batsman_stats$match_format)
australian_batsman_stats$venue             <- as.character(australian_batsman_stats$venue)
australian_batsman_stats$date              <- as.Date(australian_batsman_stats$date)
australian_batsman_stats$city              <- as.character(australian_batsman_stats$city)
australian_batsman_stats$batting_position  <- as.integer(australian_batsman_stats$batting_position) 

australian_batsman_stats$runs <- ifelse(is.na(australian_batsman_stats$runs),0,australian_batsman_stats$runs)

# to get batting position, we will group by date and then assign rank value as batting_position
#australian_batsman_stats <- australian_batsman_stats %>% group_by(date,innings_number) %>% 
#  arrange(date,innings_number) %>% dplyr::mutate(rank = rank(player_name)) %>% arrange(date,innings_number,rank)

#australian_batsman_stats$batting_position <- australian_batsman_stats$rank
#australian_batsman_stats$rank <- NULL

australia_squad <- c(  'AJ Finch',  'JP Behrendorff',  'AT Carey',  'NM Coulter-Nile',  'PJ Cummins',  'PSP Handscomb',  
                       'UT Khawaja',  'NM Lyon', 'SE Marsh',  'GJ Maxwell',  'KW Richardson',  'JA Richardson',  
                       'MP Stoinis',  'AJ Turner',  'A Zampa',  'DJM Short')

# lets get the stats for these players only
australian_squad_stats <- australian_batsman_stats %>% filter(player_name %in% australia_squad) %>% arrange(date,innings_number)
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
  
  fours_till_date <- player_stats %>% filter(date < match_stat$date) %>% select(X4s)
  if(nrow(fours_till_date) == 0){
    fours_till_date <- 0
  }else{
    fours_till_date <- sum(fours_till_date)
  }
  
  sixes_till_date <- player_stats %>% filter(date < match_stat$date) %>% select(X6s)
  if(nrow(sixes_till_date) == 0){
    sixes_till_date <- 0
  }else{
    sixes_till_date <- sum(sixes_till_date)
  }
  
  avg_strike_rate <- player_stats %>% filter(date < match_stat$date) %>% mean(strike_rate)
  
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
  
  matches_played_by_australia_last_year <- australian_team_stats %>% filter(date < match_stat$date 
                                                                            & date > date_one_year_back 
                                                                            & match_format == match_stat$match_format) %>% nrow()
  last_year_player_stats <- player_stats %>% filter(date < match_stat$date & date > date_one_year_back)
  matches_played_last_year <- nrow(last_year_player_stats)
  
  runs_scored_last_year <- sum(last_year_player_stats$runs)
  
  if(matches_played_last_year == 0)
    avg_last_year <- 0
  else
    avg_last_year <- runs_scored_last_year/matches_played_last_year
  
  ratio_of_matches_played_last_year <- matches_played_last_year / matches_played_by_australia_last_year
  centuries_last_year <- length(which(last_year_player_stats$runs > 100))
  half_centuries_last_year <- length(which(last_year_player_stats$runs > 50))
  
  australian_squad_odi_stats[match_number,"ratio_of_matches_played_last_year"] <- ratio_of_matches_played_last_year
  australian_squad_odi_stats[match_number,"runs_scored_last_year"] <- runs_scored_last_year
  australian_squad_odi_stats[match_number,"avg_last_year"] <- avg_last_year
  australian_squad_odi_stats[match_number,"centuries_last_year"] <- centuries_last_year
  australian_squad_odi_stats[match_number,"half_centuries_last_year"] <- half_centuries_last_year
  australian_squad_odi_stats[match_number,"month"] <- month(match_stat$date)
  australian_squad_odi_stats[match_number,"day"] <- day(match_stat$date)
  australian_squad_odi_stats[match_number,"total_runs_scored"] <- total_runs_scored
  australian_squad_odi_stats[match_number,"total_balls_faced"] <- total_balls_faced
  australian_squad_odi_stats[match_number,"sixes_till_date"] <- sixes_till_date
  australian_squad_odi_stats[match_number,"fours_till_date"] <- fours_till_date
  #australian_squad_odi_stats[match_number,"avg_strike_rate"] <- avg_strike_rate
  #australian_squad_odi_stats[match_number,"strike_rate_till_now"] <- strike_rate_till_now
  
}

# 1442 rows, 21 cols

venue_details <- read.csv('venue_to_hometeam_mapping.csv',stringsAsFactors = F, header = F, strip.white = T)
colnames(venue_details) <- c('venue','home_country')
venue_details <- venue_details %>% unique()

australian_squad_odi_stats <- merge(x=australian_squad_odi_stats,y=venue_details,by = 'venue') %>% arrange(date) %>% unique()
australian_squad_odi_stats$is_home_venue <- ifelse(australian_squad_odi_stats$teamName == australian_squad_odi_stats$home_country,1,0)
australian_squad_odi_stats$is_opposition_home_venue <- ifelse(australian_squad_odi_stats$opposition == australian_squad_odi_stats$home_country,1,0)


# build models for every player

# we will consider only batsmen for this analysis
player_name <- c('AJ Finch','PSP Handscomb','UT Khawaja','SE Marsh', 'GJ Maxwell','MP Stoinis','DJM Short')
fours_prediction <- as.data.frame(matrix(nrow = length(player_name),ncol = 5))
colnames(fours_prediction) <- c('player_name','lm_rmse','rf_rmse','gbrf_rmse','predicted_4s')

i <- 1
options(warn=-1)

for(player in player_name){
  
  print(player)
  df <- australian_squad_odi_stats %>% filter(player_name == player)
  player_latest_stats <- df %>% arrange(desc(date)) %>% head(1)
  
  # lets prepare the 5 odi dataset to predict on
  test_odi_df <- data.frame(matrix(nrow = 1,ncol = 15))
  colnames(test_odi_df) <- c('runs','X4s','opposition','ratio_of_matches_played_last_year','runs_scored_last_year',
                             'avg_last_year','centuries_last_year','half_centuries_last_year','month',
                             'total_runs_scored','total_balls_faced','sixes_till_date','fours_till_date',
                             'is_home_venue','is_opposition_home_venue')
  test_odi_df$opposition <- 'India'
  test_odi_df$is_home_venue <- 1
  test_odi_df$is_opposition_home_venue <- 0
  test_odi_df$X4s <- 0
  test_odi_df$ratio_of_matches_played_last_year <- player_latest_stats$ratio_of_matches_played_last_year
  test_odi_df$runs_scored_last_year <- player_latest_stats$runs_scored_last_year
  test_odi_df$avg_last_year <- player_latest_stats$avg_last_year
  test_odi_df$centuries_last_year <- player_latest_stats$centuries_last_year
  test_odi_df$half_centuries_last_year <- player_latest_stats$half_centuries_last_year
  test_odi_df$month <- 3
  test_odi_df$total_runs_scored <- player_latest_stats$total_runs_scored
  test_odi_df$total_balls_faced <- player_latest_stats$total_balls_faced
  test_odi_df$opposition <- as.factor(test_odi_df$opposition)
  test_odi_df$runs <- australia_runs_prediction[which(australia_runs_prediction$player_name == player),"predicted_runs"]
  test_odi_df$fours_till_date <- player_latest_stats$fours_till_date
  test_odi_df$sixes_till_date <- player_latest_stats$sixes_till_date
  
  df$balls_faced <- NULL
  df$player_name <- NULL
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
  
  df$opposition <- as.factor(df$opposition)
  levels(test_odi_df$opposition) <- levels(df$opposition)
  
  training_row_nos <- floor(0.8*nrow(df))
  test_row_nos <- training_row_nos + 1
  train_df <- df[1:training_row_nos,]
  test_df <- df[test_row_nos:nrow(df),]
  
  
  sapply(train_df, function(x) length(which(is.na(x))))
  
  #### Model 1: Linear regression  
  model_lm <- lm(X4s~ ratio_of_matches_played_last_year + runs +
                   runs_scored_last_year + avg_last_year + centuries_last_year + half_centuries_last_year + 
                   month + is_home_venue + is_opposition_home_venue + 
                   total_runs_scored + total_balls_faced + sixes_till_date + fours_till_date,data= train_df)
  pred <- predict(model_lm, test_df) %>% round(digits = 0)
  cbind(test_df$X4s,pred)
  RMSE(test_df$X4s,pred)
  fours_prediction$lm_rmse[i] <- RMSE(test_df$X4s,pred)
  lm_4s <- sum(pred)
  
  #### Model 2: Random Forest
  library(randomForest)
  model_rf <- randomForest(X4s~ opposition + ratio_of_matches_played_last_year + runs +
                             runs_scored_last_year + avg_last_year + centuries_last_year + half_centuries_last_year + 
                             month + is_home_venue + is_opposition_home_venue + 
                             total_runs_scored + total_balls_faced + sixes_till_date + fours_till_date
                           ,ntree = 500, data= train_df, mtry=10)
  predicted_4s <- predict(model_rf, test_df) %>% round(digits = 0)
  cbind(test_df$X4s,predicted_4s)
  RMSE(test_df$X4s,predicted_4s)
  fours_prediction$rf_rmse[i] <- RMSE(test_df$X4s,predicted_4s)
  rf_4s <- sum(predicted_4s)
  
  #### Model 3: XGBoost 
  library(xgboost)
  library(Matrix)
  
  trainm<- sparse.model.matrix(X4s~., data = train_df)
  train_label <- train_df$X4s
  train_Data <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)
  
  testm<- sparse.model.matrix(X4s~., data = test_df)
  test_label <- test_df$X4s
  test_Data <- xgb.DMatrix(data = as.matrix(testm), label = test_label)
  
  bst_model <- xgb.train(nrounds = 100, data = train_Data)
  bst_model
  
  predicted_4s_xg <- predict(bst_model, newdata = test_Data) %>% round(0)
  cbind(test_df$X4s, predicted_4s_xg)
  RMSE(test_df$X4s,predicted_4s_xg)
  fours_prediction$gbrf_rmse[i] <- RMSE(test_df$X4s,predicted_4s_xg)
  fours_prediction$player_name[i] <- player
  bst_4s <- sum(predicted_4s_xg)
  
  # choose the best model based on rmse
  best_rmse <- min(fours_prediction$gbrf_rmse[i],fours_prediction$rf_rmse[i],fours_prediction$lm_rmse[i])
  if(best_rmse == fours_prediction$gbrf_rmse[i]){
    testm<- sparse.model.matrix(X4s~., data = test_odi_df)
    test_label <- test_odi_df$X4s
    test_Data <- xgb.DMatrix(data = as.matrix(testm), label = test_label)
    prediction <- predict(bst_model, newdata = test_Data) %>% round(0)
    fours_prediction$predicted_4s[i] <- sum(prediction)
  }else{
    if(best_rmse == fours_prediction$rf_rmse[i]){
      prediction <- predict(model_rf, newdata = test_odi_df) %>% round(0)
      fours_prediction$predicted_4s[i] <- sum(prediction)
    }else{
      prediction <- predict(model_lm, newdata = test_odi_df) %>% round(0)
      fours_prediction$predicted_4s[i] <- sum(prediction)
    }
  }
  
  # cv_results <- xgb.cv(nrounds = 100, data = train_Data, nfold = 5)
  # cv_results
  i <- i + 1
  print('done')
}

options(warn=0)

fours_prediction

#   player_name   lm_rmse  rf_rmse gbrf_rmse predicted_4s
#      AJ Finch  1.596872 1.732051  1.830301            4
# PSP Handscomb  1.414214 2.081666  1.290994            3
#    UT Khawaja 88.084051 1.483240  1.949359            2
#      SE Marsh  1.860521 1.687055  2.287087            3
#    GJ Maxwell  1.054093 1.247219  1.333333            7
#    MP Stoinis  1.843909 1.095445  1.414214            1
#     DJM Short  2.000000 3.000000  1.000000            4

#######################################################################################

# build models for every player

# we will consider only batsmen for this analysis
player_name <- c('AJ Finch','PSP Handscomb','UT Khawaja','SE Marsh', 'GJ Maxwell','MP Stoinis','DJM Short')
sixes_prediction <- as.data.frame(matrix(nrow = length(player_name),ncol = 5))
colnames(sixes_prediction) <- c('player_name','lm_rmse','rf_rmse','gbrf_rmse','predicted_6s')

i <- 1
options(warn=-1)

for(player in player_name){
  
  print(player)
  df <- australian_squad_odi_stats %>% filter(player_name == player)
  player_latest_stats <- df %>% arrange(desc(date)) %>% head(1)
  
  # lets prepare the 5 odi dataset to predict on
  test_odi_df <- data.frame(matrix(nrow = 1,ncol = 15))
  colnames(test_odi_df) <- c('runs','X6s','opposition','ratio_of_matches_played_last_year','runs_scored_last_year',
                             'avg_last_year','centuries_last_year','half_centuries_last_year','month',
                             'total_runs_scored','total_balls_faced','sixes_till_date','fours_till_date',
                             'is_home_venue','is_opposition_home_venue')
  test_odi_df$opposition <- 'India'
  test_odi_df$is_home_venue <- 1
  test_odi_df$is_opposition_home_venue <- 0
  test_odi_df$X6s <- 0
  test_odi_df$ratio_of_matches_played_last_year <- player_latest_stats$ratio_of_matches_played_last_year
  test_odi_df$runs_scored_last_year <- player_latest_stats$runs_scored_last_year
  test_odi_df$avg_last_year <- player_latest_stats$avg_last_year
  test_odi_df$centuries_last_year <- player_latest_stats$centuries_last_year
  test_odi_df$half_centuries_last_year <- player_latest_stats$half_centuries_last_year
  test_odi_df$month <- 3
  test_odi_df$total_runs_scored <- player_latest_stats$total_runs_scored
  test_odi_df$total_balls_faced <- player_latest_stats$total_balls_faced
  test_odi_df$opposition <- as.factor(test_odi_df$opposition)
  test_odi_df$runs <- australia_runs_prediction[which(australia_runs_prediction$player_name == player),"predicted_runs"]
  test_odi_df$fours_till_date <- player_latest_stats$fours_till_date
  test_odi_df$sixes_till_date <- player_latest_stats$sixes_till_date
  
  df$balls_faced <- NULL
  df$player_name <- NULL
  df$X4s <- NULL
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
  
  df$opposition <- as.factor(df$opposition)
  levels(test_odi_df$opposition) <- levels(df$opposition)
  
  training_row_nos <- floor(0.8*nrow(df))
  test_row_nos <- training_row_nos + 1
  train_df <- df[1:training_row_nos,]
  test_df <- df[test_row_nos:nrow(df),]
  
  
  sapply(train_df, function(x) length(which(is.na(x))))
  
  #### Model 1: Linear regression  
  model_lm <- lm(X6s~ ratio_of_matches_played_last_year + runs +
                   runs_scored_last_year + avg_last_year + centuries_last_year + half_centuries_last_year + 
                   month + is_home_venue + is_opposition_home_venue + 
                   total_runs_scored + total_balls_faced + sixes_till_date + sixes_till_date,data= train_df)
  pred <- predict(model_lm, test_df) %>% round(digits = 0)
  cbind(test_df$X6s,pred)
  RMSE(test_df$X6s,pred)
  sixes_prediction$lm_rmse[i] <- RMSE(test_df$X6s,pred)
  lm_6s <- sum(pred)
  
  #### Model 2: Random Forest
  library(randomForest)
  model_rf <- randomForest(X6s~ opposition + ratio_of_matches_played_last_year + runs +
                             runs_scored_last_year + avg_last_year + centuries_last_year + half_centuries_last_year + 
                             month + is_home_venue + is_opposition_home_venue + 
                             total_runs_scored + total_balls_faced + sixes_till_date + sixes_till_date
                           ,ntree = 500, data= train_df, mtry=10)
  predicted_6s <- predict(model_rf, test_df) %>% round(digits = 0)
  cbind(test_df$X6s,predicted_6s)
  RMSE(test_df$X6s,predicted_6s)
  sixes_prediction$rf_rmse[i] <- RMSE(test_df$X6s,predicted_6s)
  rf_6s <- sum(predicted_6s)
  
  #### Model 3: XGBoost 
  library(xgboost)
  library(Matrix)
  
  trainm<- sparse.model.matrix(X6s~., data = train_df)
  train_label <- train_df$X6s
  train_Data <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)
  
  testm<- sparse.model.matrix(X6s~., data = test_df)
  test_label <- test_df$X6s
  test_Data <- xgb.DMatrix(data = as.matrix(testm), label = test_label)
  
  bst_model <- xgb.train(nrounds = 100, data = train_Data)
  bst_model
  
  predicted_6s_xg <- predict(bst_model, newdata = test_Data) %>% round(0)
  cbind(test_df$X6s, predicted_6s_xg)
  RMSE(test_df$X6s,predicted_6s_xg)
  sixes_prediction$gbrf_rmse[i] <- RMSE(test_df$X6s,predicted_6s_xg)
  sixes_prediction$player_name[i] <- player
  bst_6s <- sum(predicted_6s_xg)
  
  # choose the best model based on rmse
  best_rmse <- min(sixes_prediction$gbrf_rmse[i],sixes_prediction$rf_rmse[i],sixes_prediction$lm_rmse[i])
  if(best_rmse == sixes_prediction$gbrf_rmse[i]){
    testm<- sparse.model.matrix(X6s~., data = test_odi_df)
    test_label <- test_odi_df$X6s
    test_Data <- xgb.DMatrix(data = as.matrix(testm), label = test_label)
    prediction <- predict(bst_model, newdata = test_Data) %>% round(0)
    sixes_prediction$predicted_6s[i] <- sum(prediction)
  }else{
    if(best_rmse == sixes_prediction$rf_rmse[i]){
      prediction <- predict(model_rf, newdata = test_odi_df) %>% round(0)
      sixes_prediction$predicted_6s[i] <- sum(prediction)
    }else{
      prediction <- predict(model_lm, newdata = test_odi_df) %>% round(0)
      sixes_prediction$predicted_6s[i] <- sum(prediction)
    }
  }
  
  # cv_results <- xgb.cv(nrounds = 100, data = train_Data, nfold = 5)
  # cv_results
  i <- i + 1
  print('done')
}

options(warn=0)

sixes_prediction

#   player_name   lm_rmse   rf_rmse gbrf_rmse predicted_6s
#      AJ Finch  1.936492 1.2041595 1.1618950            1
# PSP Handscomb  1.414214 1.1547005 1.1547005            1
#    UT Khawaja 26.092144 0.4472136 0.6324555            0
#      SE Marsh  1.270978 1.1094004 0.9198662            0
#    GJ Maxwell  1.054093 0.8819171 1.3944334            2
#    MP Stoinis  1.549193 1.0954451 0.7745967            1
#     DJM Short  4.000000 1.0000000 1.0000000            0