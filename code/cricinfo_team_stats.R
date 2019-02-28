library('httr')
library(RCurl)
library(XML)
library(rvest)
library(purrr)

cricinfo_url <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;filter=advanced;orderby=start;size=200;team=2;template=results;type=team;view=results;wrappertype=print"

team_stats_html <- read_html(cricinfo_url)
tabular_output <- team_stats_html %>%  html_nodes("table")
page_numbers_to_navigate <- tabular_output[[2]] %>% html_nodes("td") %>% .[[1]] %>% html_nodes("b") %>% .[[2]] %>% html_text()

team_stats <- data.frame(matrix(nrow=0, ncol=6)) 

for(i in 1:page_numbers_to_navigate){
  
  print(i)
  
  urlToNavigate <- paste0("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=11;filter=advanced;orderby=start;page="
                          ,i,";size=200;team=2;template=results;type=team;view=results;wrappertype=print")
  
  team_stats_html <- read_html(urlToNavigate)
  table_tags_html <- team_stats_html %>%  html_nodes("table")
  tabular_output <- table_tags_html[[3]] %>% html_nodes("tbody") %>% html_nodes("tr")
  
  for(team_stat in tabular_output){
    
    col_data <- team_stat %>% html_nodes("td")
    
    team_name <- xml_contents(col_data[[1]]) %>% html_text() %>% as.character() %>% unlist()
    result <- xml_contents(col_data[[2]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
    match_type <- xml_contents(col_data[[8]]) %>% html_text() %>% as.character() %>% strsplit(' v ',fixed = T) %>% .[[1]] %>% .[1] %>% unlist()
    opposition	<- xml_contents(col_data[[8]]) %>% html_text() %>% as.character() %>% strsplit(' v ',fixed = T) %>% .[[1]] %>% .[2] %>% unlist()
    venue <- xml_contents(col_data[[9]]) %>% html_text() %>% as.character() %>% unlist()
    start_date <- xml_contents(col_data[[10]]) %>% html_text() %>% as.character() %>% strptime(format="%d %b %Y") %>% as.Date() %>% as.character() %>% unlist()
    
    ui_row <- cbind(team_name,result,match_type,opposition,venue,start_date)
    team_stats <- rbind(team_stats,ui_row)
    
  }
  
}

colnames(team_stats) <- c('team_name','result','match_format','opposition','venue','date')


write.csv(team_stats,'australia_team_stats.csv')



















