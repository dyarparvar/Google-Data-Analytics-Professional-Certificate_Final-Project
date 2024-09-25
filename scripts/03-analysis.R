library(tidyverse)

################### LOAD


load("rdas/data.rda") # loads the saved data object with it's own name. no need to assign to something!
View(data)



################### ANALYSE & SAVE GRAPHS

data %>% 
  mutate(abb = reorder(abb, rate)) %>%   
  ggplot() +
  geom_bar(mapping = aes(abb, rate), width = 0.5, stat = "identity", color = "black") +
  coord_flip()



ggsave("figs/01-plot.png") # this just saves the last graph


# save each graph without ggsave() 
png(file = "figs/02-plot.png", bg = "transparent") # add this line before each graph

data %>% 
  mutate(abb = reorder(abb, rate)) %>%   
  ggplot() +
  geom_bar(mapping = aes(abb, rate), width = 0.5, stat = "identity", color = "black") 

dev.off() # add this line at the end on each graph




