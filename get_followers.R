library(dplyr)
library(tidyverse)
library(rtweet)
library(jsonlite)
library(writexl)

# retrieves the target account's followers.
get_followers <- function(target_account) {
  followers <- rtweet::get_followers(
    user = target_account,
    n = num,
    retryonratelimit = TRUE
  )
  return(followers)
}

# retrieves multiple target account followers. 
get_multiple_followers <- function(target_accounts) {
  followers <- list()
  for(i in 1:length(target_accounts)) {
    followers[[i]] <- get_followers(target_accounts[i]) %>%
      mutate(network = str_replace_all(target_accounts[i], c("@" = "", "News" = "")))
  }
  tidy_df <- rbind_pages(followers) %>% as_tibble()
  return(tidy_df)
}

# creates variables for the US political parties. 
reps <- "@GOP"
dems <- "@TheDemocrats"

# creates variables for the major US broadcasters. 
fox <- "@FoxNews"
nbc <- "@NBCNews"
cbs <- "@CBSNews"
abc <- "@ABC"

# creates an object containing both of the parties.
parties <- c(reps, dems)

# creates an object containing all of the networks. 
networks <- c(fox, nbc, cbs, abc)

# calculates how many hours it would take to download all 
# the followers with rate limits (200+). 
(((2.2+2.9+21.7+17.4+8.6+9.1)*10^6)/75000)/4

# calculates how many hours it would take to download the
# latest 100,000 followers with rate limits (3). 
(((150*6)*10^3)/75000)/4

# sets the number of followers to collect to at least 100 thousand 
# and stores the network data in a tidy data frame.
num <- 10^5
party_followers <- get_multiple_followers(parties)
network_followers <- get_multiple_followers(networks)

# create a final data frame. 
final_df <- party_followers %>%
  rbind(network_followers)

# saves the data as an xlsx file to prevent the 
# user ids from being rounded (happens in other formats). 
write_xlsx(final_df, "followers_data.xlsx")

