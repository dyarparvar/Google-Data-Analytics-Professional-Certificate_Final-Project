library(tidyverse)
raw_data <- read_csv("data/raw-data.csv") # loading the file we saved into data folder
data <- raw_data %>%  mutate(region = factor(region),
                             rate = total / population * 10^5) # preparing data
save(data, file = "rdas/data.rda") # saving data object into a file in rdas folder





# While save let’s you save several objects that then get loaded with the same names used when saving, the function saveRDS lets you save one object, without the name. To bring it back you use the readRDS function. 

# saveRDS(data, file = "rdas/d.rda") 

# To bring it back you use the readRDS function.
# d <- readRDS("rdas/d.rda")
