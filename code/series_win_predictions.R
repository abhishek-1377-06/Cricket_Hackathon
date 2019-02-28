
indian_team_stats <- read.csv(file = 'india_team_stats.csv',stringsAsFactors = F)
indian_team_stats$X <- NULL
indian_team_stats$date <- as.Date(indian_team_stats$date)

india_series_stats <- read.csv(file = 'india_series_stats.csv',stringsAsFactors = F)
india_series_stats$X <- NULL
india_series_stats$start_date <- as.Date(india_series_stats$start_date)

india_series_stats <- india_series_stats %>% arrange(start_date)

venue_details <- read.csv('venue_to_hometeam_mapping.csv',stringsAsFactors = F, header = F, strip.white = T)
colnames(venue_details) <- c('venue','home_country')
venue_details <- venue_details %>% unique()

indian_team_stats <- merge(x=indian_team_stats,y=venue_details,by = 'venue') %>% arrange(date) %>% unique()

indian_team_stats$is_home_venue <- ifelse(indian_team_stats$team_name == indian_team_stats$home_country,1,0)
indian_team_stats$is_opposition_home_venue <- ifelse(indian_team_stats$opposition == indian_team_stats$home_country,1,0)
indian_team_stats <- indian_team_stats %>% filter(match_format == 'ODI')
indian_team_stats$last_match_lost_flag[1] <- 'won'

setdiff(indian_team_stats$venue, venue_details$V1)

for( match_number in 1:nrow(indian_team_stats)){
  match_stat <- indian_team_stats[match_number,]
  date_one_year_back <- match_stat$date %m-% months(12)
  
  matches_last_year <- indian_team_stats %>% filter(date < match_stat$date & date > date_one_year_back )
  losses_last_year <- matches_last_year %>% filter(result == 'lost')
  if(nrow(matches_last_year) == 0){
    loss_ratio_last_year <- 0
  }else{
    loss_ratio_last_year <- nrow(losses_last_year)/nrow(matches_last_year)
  }
  
  odi_matches_last_year <- matches_last_year %>% filter(match_format == 'ODI')
  odi_loss_last_year <- odi_matches_last_year %>% filter(result == 'lost')
  if(nrow(odi_matches_last_year) == 0){
    odi_loss_ratio_last_year <- 0
  }else{
    odi_loss_ratio_last_year <- nrow(odi_loss_last_year)/nrow(odi_matches_last_year)
  }
  
  last_year_odi_stats_vs_aus <- odi_matches_last_year %>% filter(opposition == 'Australia')
  last_year_odi_loss_against_aus <- last_year_odi_stats_vs_aus %>% filter(result == 'lost')
  if(nrow(last_year_odi_stats_vs_aus) == 0){
    last_year_odi_loss_ratio_against_aus <- 0
  }else{
    last_year_odi_loss_ratio_against_aus <- nrow(last_year_odi_loss_against_aus)/nrow(last_year_odi_stats_vs_aus)
  }
  
  overall_odi_stats_vs_aus <- indian_team_stats %>% filter(opposition == 'Australia')
  overall_odi_loss_against_aus <- overall_odi_stats_vs_aus %>% filter(result == 'lost')
  if(nrow(overall_odi_stats_vs_aus) == 0){
    overall_odi_loss_ratio_against_aus <- 0
  }else{
    overall_odi_loss_ratio_against_aus <- nrow(overall_odi_loss_against_aus)/nrow(overall_odi_stats_vs_aus)
  }
  
  overall_odi_loss <- indian_team_stats %>% filter(result == 'lost' & match_format == 'ODI')
  overall_odi_loss_ratio <- nrow(overall_odi_loss)/nrow(indian_team_stats)
  
  last_tournament_lost_flag <- india_series_stats %>% filter(start_date < match_stat$date) %>% arrange(desc(start_date)) %>% 
    head(1) %>% select(result) %>% as.character() %>% unlist()
  last_tournament_lost_flag <- ifelse(last_tournament_lost_flag == 'lost',1,0)
  
  last_match_lost_flag <- indian_team_stats %>% filter(date < match_stat$date) %>% arrange(desc(date)) %>% 
    head(1) %>% select(result) %>% as.character() %>% unlist()
  last_match_lost_flag <- ifelse(last_match_lost_flag == 'lost',1,0)
  
  indian_team_stats[match_number,"loss_ratio_last_year"] <- loss_ratio_last_year
  indian_team_stats[match_number,"odi_loss_ratio_last_year"] <- odi_loss_ratio_last_year
  indian_team_stats[match_number,"last_year_odi_loss_ratio_against_aus"] <- last_year_odi_loss_ratio_against_aus
  indian_team_stats[match_number,"overall_odi_loss_ratio_against_aus"] <- overall_odi_loss_ratio_against_aus
  indian_team_stats[match_number,"overall_odi_loss_ratio"] <- overall_odi_loss_ratio
  indian_team_stats[match_number,"last_tournament_lost_flag"] <- last_tournament_lost_flag
  indian_team_stats[match_number,"last_match_lost_flag"] <- last_match_lost_flag
  
  print(match_number)
}

# build models for every player

# we will consider only batsmen for this analysis
match_number <- c(1:5)
results_prediction <- as.data.frame(matrix(nrow = length(match_number),ncol = 5))
colnames(results_prediction) <- c('match_number','lm_accuracy','rf_accuracy','gbrf_accuracy','predicted_result')

i <- 1
options(warn=-1)
for(match in match_number){
  
  print(match)
  
  tmp_result <- 0
  test_odi_df <- data.frame(matrix(nrow = 1,ncol = 11))
  colnames(test_odi_df) <- c('opposition','is_home_venue','is_opposition_home_venue','loss_ratio_last_year',
                             'odi_loss_ratio_last_year','last_year_odi_loss_ratio_against_aus','overall_odi_loss_ratio_against_aus',
                             'overall_odi_loss_ratio','last_tournament_lost_flag','last_match_lost_flag','result')
  df <- indian_team_stats 
  df$last_match_lost_flag <- as.integer(df$last_match_lost_flag)
  df$result <- ifelse(df$result == 'won',1,0)
  
  match_latest_stats <- df %>% arrange(desc(date)) %>% head(1)
  
  date_one_year_back <- df$date[match] %m-% months(12)
  
  last_year_matches <- df %>% filter(date < df$date[match] & date > date_one_year_back )
  
  test_odi_df$opposition <- 'Australia'
  test_odi_df$is_home_venue <- 1
  test_odi_df$is_opposition_home_venue <- 0
  test_odi_df$result <- 'won' # this is what we will predict
  
  last_year_losses <- match_latest_stats$loss_ratio_last_year * nrow(last_year_matches)
  test_odi_df$loss_ratio_last_year <- (last_year_losses + ifelse(match_latest_stats$result == 'lost',1,0))/(nrow(df) + 1) 
  
  last_year_odi_losses <- match_latest_stats$odi_loss_ratio_last_year * nrow(last_year_matches)
  test_odi_df$odi_loss_ratio_last_year <- (last_year_odi_losses + ifelse(match_latest_stats$result == 'lost',1,0))/(nrow(df) + 1) 
  
  df1 <- last_year_matches %>% filter(opposition == 'Australia')
  last_year_odi_losses_against_aus <- match_latest_stats$last_year_odi_loss_ratio_against_aus * nrow(df1)
  test_odi_df$last_year_odi_loss_ratio_against_aus <- (last_year_odi_losses_against_aus + 
                                                         ifelse(match_latest_stats$result == 'lost',1,0))/(nrow(df1) + 1)
  
  overall_odi_losses_against_aus <- match_latest_stats$overall_odi_loss_ratio_against_aus * nrow(df1)
  test_odi_df$overall_odi_loss_ratio_against_aus <- (overall_odi_losses_against_aus + 
                                                       ifelse(match_latest_stats$result == 'lost',1,0))/(nrow(df1) + 1)
  
  #overall_odi_loss_ratio
  overall_odi_lossess <- match_latest_stats$overall_odi_loss_ratio * nrow(df)
  test_odi_df$overall_odi_loss_ratio <- (overall_odi_lossess + 
                                           ifelse(match_latest_stats$result == 'lost',1,0))/(nrow(df) + 1)
  
  test_odi_df$last_tournament_lost_flag <- match_latest_stats$last_tournament_lost_flag
  test_odi_df$last_match_lost_flag <- tmp_result
  
  test_odi_df$opposition <- as.factor(test_odi_df$opposition)
  df$opposition <- as.factor(df$opposition)
  levels(test_odi_df$opposition) <- levels(df$opposition)
  
  df$venue <- NULL
  df$team_name <-NULL
  df$match_format <- NULL
  df$date <- NULL
  df$home_country <- NULL
  
  training_row_nos <- floor(0.8*nrow(df))
  test_row_nos <- training_row_nos + 1
  train_df <- df[1:training_row_nos,]
  test_df <- df[test_row_nos:nrow(df),]
  
  sapply(train_df, function(x) length(which(is.na(x))))
  test_odi_df$opposition <- 'Australia'
  
  library(caTools)
  set.seed(101)
  #indices = sample.split(df$result, SplitRatio = 0.8)
  #train_df = df[indices,]
  #test_df = df[!(indices),]
  
  
  #### Model 1: Linear regression  
  model_lm <- glm(result ~ is_home_venue + is_opposition_home_venue + loss_ratio_last_year + 
                   odi_loss_ratio_last_year + last_year_odi_loss_ratio_against_aus + overall_odi_loss_ratio_against_aus
                 + overall_odi_loss_ratio + last_tournament_lost_flag + last_match_lost_flag ,data= train_df)
  pred <- predict(model_lm, test_df) %>% round(digits = 0)
  cbind(test_df$result,pred)
  cf <- confusionMatrix(as.factor(test_df$result),as.factor(pred))
  results_prediction$lm_accuracy[i] <- cf[[3]][1]
  
  #### Model 2: Random Forest
  library(randomForest)
  model_rf <- randomForest(result ~ opposition + is_home_venue + is_opposition_home_venue + loss_ratio_last_year + 
                             odi_loss_ratio_last_year + last_year_odi_loss_ratio_against_aus + overall_odi_loss_ratio_against_aus
                           + overall_odi_loss_ratio + last_tournament_lost_flag + last_match_lost_flag
                           ,ntree = 500, data= train_df, mtry=10)
  predicted_results <- predict(model_rf, test_df) %>% round(digits = 0)
  cbind(test_df$result,predicted_results)
  cf <- confusionMatrix(as.factor(test_df$result),as.factor(predicted_results))
  results_prediction$rf_accuracy[i] <- cf[[3]][1]
  
  #### Model 3: XGBoost 
  library(xgboost)
  library(Matrix)
  
  trainm<- sparse.model.matrix(result ~., data = train_df)
  train_label <- train_df$result
  train_Data <- xgb.DMatrix(data = as.matrix(trainm), label = train_label)
  
  testm<- sparse.model.matrix(result ~., data = test_df)
  test_label <- test_df$result
  test_Data <- xgb.DMatrix(data = as.matrix(testm), label = test_label)
  
  bst_model <- xgb.train(nrounds = 100, data = train_Data)
  bst_model
  
  predicted_results_xg <- predict(bst_model, newdata = test_Data) %>% round(0)
  cbind(test_df$result, predicted_results_xg)
  cf <- confusionMatrix(as.factor(test_df$result),as.factor(predicted_results))
  results_prediction$gbrf_accuracy[i] <- cf[[3]][1]
  results_prediction$match_number[i] <- match
  
  # choose the best model based on accuracy
  best_accuracy <- max(results_prediction$gbrf_accuracy[i],results_prediction$rf_accuracy[i],results_prediction$lm_accuracy[i])
  if(best_accuracy == results_prediction$gbrf_accuracy[i]){
    testm<- sparse.model.matrix(result~., data = test_odi_df)
    test_label <- test_odi_df$result
    test_Data <- xgb.DMatrix(data = as.matrix(testm), label = test_label)
    prediction <- predict(bst_model, newdata = test_Data) %>% round(0)
    results_prediction$predicted_results[i] <- sum(prediction)
  }else{
    if(best_accuracy == results_prediction$rf_accuracy[i]){
      prediction <- predict(model_rf, newdata = test_odi_df) %>% round(0)
      results_prediction$predicted_results[i] <- sum(prediction)
    }else{
      prediction <- predict(model_lm, newdata = test_odi_df) %>% round(0)
      results_prediction$predicted_results[i] <- sum(prediction)
    }
  }
  
  # cv_results <- xgb.cv(nrounds = 100, data = train_Data, nfold = 5)
  # cv_results
  tmp_result <- prediction
  i <- i + 1
  print('done')
}

options(warn=0)

# the run predictions are as follows --
results_prediction[,c("match_number","predicted_results")]

# match_number predicted_results
#            1                 1
#            2                 1
#            3                 1
#            4                 1
#            5                 1

write.csv(wickets_prediction,'india_results_prediction')

