library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(jsonlite)
library(ggthemes)
library(tidytext)

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

# reads in the timelines data. 
timelines_data <- read_xlsx("timelines_data.xlsx") %>%
  mutate(network = str_replace_all(network, c("The" = "", 
                                              "GOP" = "Republicans",
                                              "Fox" = "FOX")))

# lists the topics to explore. 
topics <- c("Democrat",
            "progressives",
            "Republican",
            "GOP",
            "Biden",
            "Trump")

# returns a data frame of text for all topics and networks. 
get_topics_data <- function(data, topics) {
  text <- list()
  for(i in 1:length(topics)) {
    text[[i]] <- data[str_detect(data$text, topics[i]),] %>%
      mutate(topic = topics[i])
  }
  final_df <- rbind_pages(text) %>% as_tibble()
  return(final_df)
}

# stores the data frame of text, topics and networks. 
topics_data <- get_topics_data(timelines_data, topics) %>%
  mutate(topic = str_replace_all(topic, c("Democrat" = "Democrats",
                                          "progressives" = "Democrats",
                                          "Republican" = "Republicans",
                                          "GOP" = "Republicans")))

# gets the sentiment for the text column of given data frames. 
get_sentiment <- function(data) {
  sentiment <- get_sentiments("nrc") %>% 
    select(word, sentiment)
  words <- data %>%
    unnest_tokens(word, text) %>%
    left_join(sentiment, by = "word") %>%
    mutate(sentiment = replace_na(sentiment, replace = "none")) %>%
    filter(!word %in% stop_words$word) %>%
    group_by(network, topic) %>%
    count(sentiment) %>%
    rename(words = "n")
  return(words)
}

# stores the sentiment values grouped by network and topic.  
sentiment <- get_sentiment(topics_data)

# creates a barplot for the timelines data. 
get_timelines_barplot <- function(data, topics, label=NULL) {
  barplot <- data %>%
    filter(topic %in% topics) %>%
    spread(key = sentiment, value = words, -c(1,2)) %>%
    group_by(network) %>%
    mutate(total = (positive+negative)) %>%
    group_by(network, topic) %>%
    mutate(Net = ((positive-negative)/total)*100) %>%
    ggplot(aes(x=network,y=Net,fill=network)) +
    geom_bar(stat = "identity", position = "dodge") +
    geom_text(aes(label=ifelse(network=="FOX",round(Net),"")),
              size=3 ,
              col="#696969",
              position = position_stack(vjust = 0.5)) +
    ggtitle(label) +
    xlab("") +
    ylab("Net Sentiment (%)") +
    facet_wrap(~topic) +
    my_theme +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank())
  return(barplot)
}

# shows the sentiment values for for the parties timeline data. 
Parties <- c("Democrats", "Republicans")
get_timelines_barplot(data=sentiment, 
                      topics=Parties,
                      label="Parties")

# shows the sentiment values for for the politicians timeline data. 
Politicians <- c("Biden", "Trump")
get_timelines_barplot(data=sentiment, 
                      topics=Politicians,
                      label="Politicians")

