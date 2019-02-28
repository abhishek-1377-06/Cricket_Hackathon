library('httr')
library(RCurl)
library(XML)
library(rvest)
library(purrr)

cricinfo_url <- "http://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;page=1;team=2;template=results;type=team;view=series;wrappertype=print"

series_stats_html <- read_html(cricinfo_url)
tabular_output <- series_stats_html %>%  html_nodes("table")
page_numbers_to_navigate <- tabular_output[[2]] %>% html_nodes("td") %>% .[[1]] %>% html_nodes("b") %>% .[[2]] %>% html_text()

series_stats <- data.frame(matrix(nrow=0, ncol=7)) 

for(i in 1:page_numbers_to_navigate){
  
  print(i)
  
  urlToNavigate <- paste0("http://stats.espncricinfo.com/ci/engine/stats/index.html?class=1;page="
                          ,i,";team=2;template=results;type=team;view=series;wrappertype=print")
  
  series_stats_html <- read_html(urlToNavigate)
  table_tags_html <- series_stats_html %>%  html_nodes("table")
  tabular_output <- table_tags_html[[3]] %>% html_nodes("tbody") %>% html_nodes("tr")
  
  for(series_stat in tabular_output){
    
    col_data <- series_stat %>% html_nodes("td")
    if(length(col_data) > 1){
      team_name <- xml_contents(col_data[[1]]) %>% html_text() %>% as.character() %>% strsplit('(',fixed = T) %>% pluck(1) %>% .[1] %>% unlist()
      matches_played <- xml_contents(col_data[[2]]) %>% html_text() %>% as.character() %>% unlist()
      matches_lost <- xml_contents(col_data[[4]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
      loss_ratio <- as.integer(matches_lost)/as.integer(matches_played)
      winner <- xml_contents(col_data[[15]]) %>% html_text() %>% as.character() %>% .[1] %>% unlist()
      result <- ifelse(winner == team_name,'won','lost')
      start_date <- xml_contents(col_data[[13]]) %>% html_text() %>% as.character() %>% strptime(format="%d %b %Y") %>% as.Date() %>% as.character()  %>% unlist()
      print(start_date)
      
      ui_row <- cbind(team_name,matches_played,matches_lost,loss_ratio,winner,result,start_date)
      series_stats <- rbind(series_stats,ui_row)
    }
    
  }
  
}

colnames(series_stats) <- c('team_name','matches_played','matches_lost','loss_ratio','winner','result','start_date')

write.csv(series_stats,'australia_series_stats.csv')

