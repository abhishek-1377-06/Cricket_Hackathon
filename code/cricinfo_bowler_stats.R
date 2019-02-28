library('httr')
library(RCurl)
library(XML)
library(rvest)
library(purrr)

cricinfo_url <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;filter=advanced;team=2;page=1;orderby=wickets;size=200;template=results;type=bowling;view=innings;wrappertype=print"

bowler_stats_html <- read_html(cricinfo_url)
tabular_output <- bowler_stats_html %>%  html_nodes("table")
page_numbers_to_navigate <- tabular_output[[2]] %>% html_nodes("td") %>% .[[1]] %>% html_nodes("b") %>% .[[2]] %>% html_text()

bowler_stats <- data.frame(matrix(nrow=0, ncol=14)) 

for(i in 1:page_numbers_to_navigate){
  
  print(i)
  
  urlToNavigate <- paste0("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;filter=advanced;team=2;page="
                          ,i,";orderby=wickets;size=200;template=results;type=bowling;view=innings;wrappertype=print")
  
  bowler_stats_html <- read_html(urlToNavigate)
  table_tags_html <- bowler_stats_html %>%  html_nodes("table")
  tabular_output <- table_tags_html[[3]] %>% html_nodes("tbody") %>% html_nodes("tr")
  
  for(player_stat in tabular_output){
    
    col_data <- player_stat %>% html_nodes("td")
    
    player_name <- xml_contents(col_data[[1]]) %>% html_text() %>% as.character() %>% strsplit('(',fixed = T) %>% pluck(1) %>% .[1] %>% unlist()
    team_name <- xml_contents(col_data[[1]]) %>% html_text() %>% as.character() %>% strsplit('(',fixed = T) %>% 
      .[[1]] %>% .[2] %>% strsplit(')',fixed = T) %>% .[[1]] %>% unlist()
    overs <- xml_contents(col_data[[2]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    maidens <- xml_contents(col_data[[4]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    runs_given <- xml_contents(col_data[[5]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    wickets <- xml_contents(col_data[[6]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    economy <- xml_contents(col_data[[7]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    innings_no <- xml_contents(col_data[[8]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    opposition	<- xml_contents(col_data[[10]]) %>% html_text() %>% as.character() %>% strsplit(' v ',fixed = T) %>% .[[1]] %>% .[2] %>% unlist()
    match_type <- xml_contents(col_data[[10]]) %>% html_text() %>% as.character() %>% strsplit(' v ',fixed = T) %>% .[[1]] %>% .[1] %>% unlist()
    venue <- xml_contents(col_data[[11]]) %>% html_text() %>% as.character() %>% unlist()
    start_date <- xml_contents(col_data[[12]]) %>% html_text() %>% as.character() %>% strptime(format="%d %b %Y") %>% as.Date() %>% as.character() %>% unlist()
    
    ui_row <- cbind(player_name,team_name,overs,maidens,runs_given,wickets,economy,
                    innings_no,opposition,match_type,venue,start_date,'','')
    bowler_stats <- rbind(bowler_stats,ui_row)
    
  }
  
}

colnames(bowler_stats) <- c('player_name','teamName','overs','maidens','runs_given','wickets','economy',
                             'innings_number','opposition','match_format','venue','date','city','batting_position')


write.csv(bowler_stats,'australia_bowler_stats.csv')



















