library('httr')
library(RCurl)
library(XML)
library(rvest)
library(purrr)

cricinfo_url <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;filter=advanced;page=1;team=2;size=200;template=results;type=batting;view=innings;wrappertype=print"

bastman_stats_html <- read_html(cricinfo_url)
tabular_output <- bastman_stats_html %>%  html_nodes("table")
page_numbers_to_navigate <- tabular_output[[2]] %>% html_nodes("td") %>% .[[1]] %>% html_nodes("b") %>% .[[2]] %>% html_text()

batsman_stats <- data.frame(matrix(nrow=0, ncol=14)) 

# 359,470,508,512 threw error
for(i in 1:page_numbers_to_navigate){
  
  print(i)
  
  urlToNavigate <- paste0("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;filter=advanced;page="
        ,i,";team=2;size=200;template=results;type=batting;view=innings;wrappertype=print")
  
  bastman_stats_html <- read_html(urlToNavigate)
  table_tags_html <- bastman_stats_html %>%  html_nodes("table")
  tabular_output <- table_tags_html[[3]] %>% html_nodes("tbody") %>% html_nodes("tr")
  
  for(player_stat in tabular_output){
    
    col_data <- player_stat %>% html_nodes("td")
    
    player_name <- xml_contents(col_data[[1]]) %>% html_text() %>% as.character() %>% strsplit('(',fixed = T) %>% pluck(1) %>% .[1] %>% unlist()
    team_name <- xml_contents(col_data[[1]]) %>% html_text() %>% as.character() %>% strsplit('(',fixed = T) %>% 
      .[[1]] %>% .[2] %>% strsplit(')',fixed = T) %>% .[[1]] %>% unlist()
    runs <- xml_contents(col_data[[2]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    balls_faced <- xml_contents(col_data[[4]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    fours <- xml_contents(col_data[[5]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    sixes <- xml_contents(col_data[[6]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    strike_rate <- xml_contents(col_data[[7]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    innings_no <- xml_contents(col_data[[8]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    opposition	<- xml_contents(col_data[[10]]) %>% html_text() %>% as.character() %>% strsplit(' v ',fixed = T) %>% .[[1]] %>% .[2] %>% unlist()
    match_type <- xml_contents(col_data[[10]]) %>% html_text() %>% as.character() %>% strsplit(' v ',fixed = T) %>% .[[1]] %>% .[1] %>% unlist()
    venue <- xml_contents(col_data[[11]]) %>% html_text() %>% as.character() %>% unlist()
    start_date <- xml_contents(col_data[[12]]) %>% html_text() %>% as.character() %>% strptime(format="%d %b %Y") %>% as.Date() %>% as.character()  %>% unlist()
    
    ui_row <- cbind(player_name,team_name,runs,balls_faced,fours,sixes,strike_rate,
                    innings_no,opposition,match_type,venue,start_date,'','')
    batsman_stats <- rbind(batsman_stats,ui_row)
    
  }
  
}

colnames(batsman_stats) <- c('player_name','teamName','runs','balls_faced','4s','6s','strike_rate',
                             'innings_number','opposition','match_format','venue','date','city','batting_position')

write.csv(batsman_stats,'australia_batsman_stats.csv')














