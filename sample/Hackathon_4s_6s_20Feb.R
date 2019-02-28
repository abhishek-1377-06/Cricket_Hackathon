library(yaml)
library(dplyr)
setwd("~/Cricket_Hackathon")

# get all the yaml files having match info
file_names <- list.files(path="~/Cricket_Hackathon")
overall_batsman_stats <- data.frame(matrix(nrow=0, ncol=13)) 
colnames(overall_batsman_stats) <- c('teamName','opposition','player_name','match_format','city','venue','date',
                                     'runs','4s','6s','balls_faced','strike_rate','batting_position')

# loop thru the files one by one. For every match, build the stats
for(input_file in file_names){
  
  if(grepl('yaml', input_file)){
    df <- read_yaml(file = input_file)
    print(input_file)
    
    # df <- read_yaml(file='1000855.yaml')
    
    general_match_stats <- unlist(df$info) %>% t() %>% as.data.frame()
    team_names <- c(as.character(general_match_stats$teams1),as.character(general_match_stats$teams2))
    
    # loop thru the innings one by one and build the batsman stats
    for(innings in df$innings){
      
      innings_info_ballwise <- innings[[1]]$deliveries
      runs_info <- data.frame(matrix(nrow=length(innings_info_ballwise), ncol=10)) 
      colnames(runs_info) <- c('player_name','runs','isFour','isSix','teamName','opposition','match_format','city','venue','date')
      runs_info$teamName <- innings[[1]]$team
      runs_info$opposition <- setdiff(team_names,runs_info$teamName)
      runs_info$match_format <- general_match_stats$match_type
      runs_info$city <- as.character(runs_info$city)
      runs_info$city <- ifelse(is.null(general_match_stats$city),'Not Available',as.character(general_match_stats$city))
      runs_info$venue <- general_match_stats$venue
      runs_info$date <- general_match_stats[,2]
      
      
      for(i in 1:length(innings_info_ballwise)){
        runs_info$player_name[i] <- innings_info_ballwise[[i]][[1]]$batsman
        
        # if it's a wide, do not count that ball
        isWide <- 0
        isExtraDelivery <- if_else(!is.null(innings_info_ballwise[[i]][[1]]$extras),1,0)
        
        if(isExtraDelivery == 1){
          isWide <- if_else(!is.null(innings_info_ballwise[[i]][[1]]$extras$wides),1,0)
        }
        
        if(isWide == 0){
          runs_info$runs[i] <- innings_info_ballwise[[i]][[1]]$runs$batsman
          if(runs_info$runs[i] == 4){
            runs_info$isFour[i] <- 1
          }else
          {runs_info$isFour[i] <- 0}
          
          if(runs_info$runs[i] == 6){
            runs_info$isSix[i] <- 1
          }else
          {runs_info$isSix[i] <- 0}
        }
        
      }
      
      # now, for every batsman, lets create a data frame with following columns -- name, position, 4s, 6s, strike_rate
      runs_info <- na.omit(runs_info)
      batsman_stats <- runs_info %>% group_by(teamName,opposition,player_name,match_format,city,venue,date) %>% summarise_all(.funs = sum) %>% as.data.frame()
      balls_faced <- runs_info %>% group_by(teamName,opposition,player_name,match_format,city,venue,date) %>% summarise(balls_faced = n())
      batsman_stats <- cbind(batsman_stats,balls_faced$balls_faced)
      batsman_stats$strike_rate <- round((batsman_stats$runs/batsman_stats$balls_faced)*100,2)
      batting_position <- batsman_stats %>% mutate(rank = dense_rank((player_name)))
      batsman_stats$batting_position <- batting_position$rank
      colnames(batsman_stats) <- c('teamName','opposition','player_name','match_format','city','venue','date',
                                   'runs','4s','6s','balls_faced','strike_rate','batting_position')
      overall_batsman_stats <- rbind(overall_batsman_stats,batsman_stats)
      
    }
    
  }
  
}

write.csv(overall_batsman_stats,'batsman_stats')
