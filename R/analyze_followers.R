library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggthemes)

# my personal plot theme for data visualizations. 
my_theme <- theme_economist_white(gray_bg = FALSE) +
  theme(plot.title = element_text(hjust = 0.5,
                                  vjust = 10,
                                  size = 10,
                                  color = "#474747"),
        plot.margin = unit(c(1.5, 1, 1.5, 1), "cm"),
        axis.text = element_text(size = 9,
                                 color = "gray30"),
        axis.text.x=element_text(vjust = -2.5),
        axis.title.x = element_text(size = 9,
                                    color = "gray30",
                                    vjust = -10),
        axis.title.y = element_text(size = 9,
                                    color = "gray30",
                                    vjust = 10),
        legend.direction = "vertical", 
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 11,
                                   color = "gray20"),
        legend.margin=margin(1, -15, 1, 0),
        legend.spacing.x = unit(0.25, "cm"),
        legend.key.size = unit(1, "cm"), 
        legend.key.height = unit(0.75, "cm"),
        strip.text = element_text(hjust = 0.5,
                                  vjust = 1,
                                  size = 10,
                                  color = "#474747"),
        panel.spacing = unit(2, "lines"))

# reads in the followers data. 
followers_data <- read_xlsx("followers_data.xlsx") %>%
  mutate(network = str_replace_all(network, c("The" = "", 
                                              "GOP" = "Republicans",
                                              "Fox" = "FOX")))

# creates a data frame with the ratio of total network followers 
# for each of the political parties. 
dems <- sum(followers_data$user_id %in% followers_data[followers_data$network=="Democrats",]$user_id)
reps <- sum(followers_data$user_id %in% followers_data[followers_data$network=="Republicans",]$user_id)
total_data <- tibble(dem_ratio =dems/(dems+reps),
                     rep_ratio =reps/(dems+reps)) %>%
  gather(key=ratio, value=value)

# creates a barplot showing the total percentage of all networks' 
# followers who are also recent followers of a political party. 
total_data %>% 
  mutate(ratio = str_replace_all(ratio, c("_" = " ",
                                          "ratio" = "",
                                          "total" = "",
                                          "party" = "",
                                          "dem" = "Democrats",
                                          "rep" = "Republicans"))) %>%
  ggplot(aes(x=ratio,
                          y=value*100,
                          fill=ratio)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 50,
             size=0.25,
             col="#696969") +
  geom_text(aes(y=52, 
                label="50%", 
                x=2.75,
                hjust=1.05),
            size=2.575,
            col="#696969") +
  ggtitle("Total Followers") +
  xlab("") +
  ylab("Followers (%)") +
  my_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

# creates an object containing the networks. 
networks <- c("ABC", "CBS", "NBC", "FOX")

# gets the ratio of media followers in the followers of a certain party. 
get_ratio <- function(data, media, party) {
  ratio <- sum(data[data$network==media,]$user_id %in% data[data$network==party,]$user_id)
  return(ratio)
}

# calculates the total and party ratios of democrats 
# to republicans for each network. 
get_ratios <- function(data, media) {
  ratios <- list()
  for(i in 1:length(networks)) {
    dems <- get_ratio(data, media[i], "Democrats")
    reps <- get_ratio(data, media[i], "Republicans")
    dem_party_ratio <- dems/(dems+reps)
    rep_party_ratio <- reps/(dems+reps)
    ratios[[i]] <- list(dem_party_ratio,
                        rep_party_ratio)
  }
  networks_list <- list()
  for(i in 1:length(media)) {
    networks_list[[i]] <- rep(media[i], 2)
  }
  final_df <- tibble(value = unlist(ratios),
                     ratio = rep(c("dem_party_ratio", 
                                   "rep_party_ratio"),4),
                     media = unlist(networks_list))
  return(final_df)
}
# gets the democratic follower ratio for the 4 major networks. 
network_ratios <- get_ratios(followers_data, networks)

# creates a barplot showing the percentage of each network's 
# followers who are also recent followers of a political party. 
network_ratios %>%
  mutate(ratio = str_replace_all(ratio, c("_" = " ",
                                          "ratio" = "",
                                          "total" = "",
                                          "party" = "",
                                          "dem" = "Democrats",
                                          "rep" = "Republicans"))) %>%
  ggplot(aes(x=media,
             y=value*100,
             fill=media)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 50,
             size=0.25,
             col="#696969") +
  geom_text(aes(y=52, 
                label="50%", 
                x=5.25,
                hjust=1.05),
            size=2.75,
            col="#696969") +
  ggtitle("Network Followers") +
  xlab("") +
  ylab("Followers (%)") +
  facet_wrap(~ratio) +
  my_theme +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

