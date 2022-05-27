visualize_numerical <- function(data, feature, label = HeartDisease){
  feature <- enquo(feature)
  label <- enquo(label)
  
  visualize_numerical_distribution <- function(data, feature){
    feature <- enquo(feature)
    
    data %>%
      ggplot() +
      geom_histogram(mapping = aes(x = !!feature), bins = 10)
  }
  
  visualize_numerical_relationship <- function(data, feature, label = HeartDisease){
    feature <- enquo(feature)
    label <- enquo(label)
    
    data %>%
      ggplot() +
      geom_density(mapping = aes(x = !!feature, fill = !!label),
                   alpha = 0.5, adjust = 4)
    
  }
  
  ggarrange(
    data %>% visualize_numerical_distribution(!!feature),
    data %>% visualize_numerical_relationship(!!feature, !!label),
    nrow = 1,
    ncol = 2,
    common.legend = TRUE, 
    legend = "bottom"
  )
}