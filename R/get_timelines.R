library(dplyr)
library(tidyverse)
library(rtweet)
library(jsonlite)
library(writexl)

# creates variables for the major US broadcasters. 
fox <- "@FoxNews"
nbc <- "@NBCNews"
cbs <- "@CBSNews"
abc <- "@ABC"

# creates an object containing all of the networks. 
networks <- c(fox, nbc, cbs, abc)

# retrieves the target account's timeline. 
get_timeline <- function(target_account) {
  timeline <- rtweet::get_timeline(
    user = target_account,
    n = 3200
  )
  return(timeline)
}

# retrieves multiple target account timelines.  
get_multiple_timelines <- function(target_accounts) {
  timelines <- list()
  for(i in 1:length(target_accounts)) {
    timelines[[i]] <- get_timeline(target_accounts[i])[,"text"] %>%
      mutate(network = str_replace_all(target_accounts[i], c("@" = "", "News" = "")))
  }
  tidy_df <- rbind_pages(timelines) %>% as_tibble()
  return(tidy_df)
}

# stores the network timelines data in a tidy data frame.
network_timelines <- get_multiple_timelines(networks)

# saves the data as an xlsx file to prevent the 
# user ids from being rounded (happens in other formats). 
write_xlsx(network_timelines, "timelines_data.xlsx")
