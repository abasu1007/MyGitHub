library(dplyr)
library(ggplot2)

# Select those colors that appear over 10 times
over_ten_colors <- pigeon %>% 
  group_by(Color) %>% 
  summarize(count = n()) %>% 
  filter(count > 10)

# Calculate average speed for these colors
average_speed_per_color <- pigeon %>% 
  filter(Color %in% over_ten_colors$Color) %>%
  group_by(Color) %>%
  summarise(Average_Speed = mean(Speed))

# Plot Speed
ggplot(pigeon, aes(x = Pos, y = Speed)) +
  geom_point() +
  xlab("Rank") + 
  ylab("Pigeon Speed") + 
  ggtitle("The correlation between speed and rank")


library(ggplot2)
# Recreate the graph from scratch and show us your skills

ggplot (chopstick_data, aes(Food_Pinching_Efficiency, fill = Chopstick_Length)) +
  geom_density(alpha = 0.3)+
  xlab("Food Pinching Efficiency") +
  ylab("Relative Frequency")

library(dplyr)
library(ggplot2)

# Yearly Production Of Silver
ggplot(silver, aes(year, silver_minted)) + geom_area (fill = 'blue', alpha = 0.3) 

# Yearly Production Of Silver & Cumulative Production of Silver Between 1720 - 1800 
silver_cs <- silver %>% cbind (cumsum = cumsum(silver$silver_minted))
  
ggplot(silver_cs, aes(year, silver_minted)) + 
geom_area (aes(y = silver_minted), fill = 'blue', alpha = 0.3) + 
geom_area (aes(y = cumsum), fill = '#c0c0c0', alpha = 0.5)

library(dplyr)
library(choroplethrMaps)

# Get the average price per state
average_weed_price <- weed %>% 
  group_by(state) %>%
  summarise(mean_price = mean(price))

# which states are most expensive?
colnames(average_weed_price) <- c("region", "value")
state_choropleth(average_weed_price, title = "Average Weed Price Per State", legend = "Price in $")
