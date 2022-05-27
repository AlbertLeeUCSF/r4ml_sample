library(tidyverse)
library(ggpubr)

visualize_categorical <- function(data, feature, label = HeartDisease){
  feature <- enquo(feature)
  label <- enquo(label)
  
  visualize_categorical_distribution <- function(data, feature){
    feature <- enquo(feature)
    data %>%
      ggplot() +
      geom_bar(mapping = aes(y = !!feature))
  }
  
  visualize_categorical_relationship <- function(data, feature, label = HeartDisease){
    feature <- enquo(feature)
    label <- enquo(label)
    
    data %>%
      select(!!feature, !!label) %>%
      group_by(!!feature, !!label) %>%
      summarize(Count = n()) %>%
      ggplot() + 
      geom_bar(mapping = aes(y = !!feature, x = Count, fill = !!label),
               position = "fill",
               stat="identity")
  }
  
  ggarrange(
    data %>% visualize_categorical_distribution(!!feature),
    data %>% visualize_categorical_relationship(!!feature, !!label),
    nrow = 1,
    ncol = 2,
    common.legend = TRUE, 
    legend = "bottom"
  )
}

