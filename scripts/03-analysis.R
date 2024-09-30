# Data Analysis
cat("Performing data analysis...\n")


################### Usage

# Save the summary to the output folder if required
# summary_stats <- summary(dailyUsage)
# write.csv(summary_stats, file.path(output_dir, "dailyUsage_stats.csv"))
# 
# dailyUsage <- activityData %>% 
#   group_by(Id) %>% 
#   summarise(mean_usage = mean(daily_usage)) %>% 
#   mutate(daily_usage = case_when(
#     mean_usage == "100" ~ "whole-day", # Q3
#     mean_usage >= "85.46" ~ "high", # Q2
#     mean_usage >= "68.61" ~ "medium", # Q1
#     TRUE ~ "low",
#   ))
# 


dailyUsage <- activityData %>% 
  group_by(Id) %>% 
  summarise(mean_usage = mean(daily_usage), sd_usage = sd(daily_usage)) %>% 
  drop_na()

mean <- mean(dailyUsage$mean_usage)
sd <- sd(dailyUsage$sd_usage)

dailyUsage <- dailyUsage %>% 
  mutate(daily_usage = case_when(
    mean_usage == "100" & sd_usage == "0" ~ "whole-time",
    mean_usage < (mean - sd) ~ "low",
    mean_usage >= (mean - sd) & mean_usage <= (mean + sd) ~ "medium",
    mean_usage > (mean + sd) ~ "high"
  ))

desired_order <- c("low", "medium", "high", "whole-time")  # Specify the order
dailyUsage <- dailyUsage %>%
  mutate(daily_usage = factor(daily_usage, levels = desired_order)) %>%
  arrange(daily_usage)





# distribution of daily usage - not grouped by Id
activityData %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = daily_usage), stat="bin", binwidth = 5) +
  labs(title = "distribution of daily usage - not grouped by Id", x = "usage", y = "frequency") +
  theme_minimal()

# distribution of daily usage - grouped by Id
dailyUsage %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = mean_usage), stat="bin", binwidth = 5) +
  labs(title = "distribution of daily usage - grouped by Id", x = "usage", y = "frequency") +
  theme_minimal()



# Save each graph

# add the following line before each graph
# png(file.path(output_dir, "daily_usage.png"), bg = "transparent", width = 800, height = 600) # Set width and height as needed
# the difference between above graphs shows that we have users that don't have consistent behaviour!!

# p1 <- dailyUsage %>% 
#   ggplot() +
#   geom_violin(mapping = aes(x = "", y = mean_usage), fill = "lightblue") +
#   geom_violin(mapping = aes(x = "", y = sd_usage), fill = "lightyellow") +
#   labs(title = "dailyUsage", x = "", y = "Usage") +
#   theme_minimal()

# p1 + geom_violin(data = activityData, mapping = aes(x = "", y = daily_usage), fill = "lightcoral") 


# dev.off() # add this line at the end on each graph



png(file.path(output_dir, "daily_usage.png"), bg = "transparent", width = 800, height = 600) # Set width and height as needed  

dailyUsage %>% 
  ggplot() +
  geom_bar(mapping = aes(x = daily_usage, fill = daily_usage), width = 10/13) +
  labs(title = "daily usage", x = "daily usage", y = "number of users")


dev.off() 



################### Sleep

# Dot plot



sleepData %>% 
  group_by(logId) %>% 
  group_by(Id) %>% 
  mutate(Start = as_hms(Start)) %>% 
  mutate(End = as_hms(End)) %>% 
  mutate(Id = format(Id, scientific = FALSE)) %>%
  arrange(Start) %>% 
  # select(index, logId, Start, End) %>% 
  ggplot() +
  # Line connecting Start and End
  geom_segment(aes(x = meanStart, xend = meanStart + meanDuration, y = Id, yend = Id), color = "grey", size = 1) +
  geom_point(mapping = aes(x = Start, y = Id), size = 3, color = "blue", shape = 21,  fill = NA) +   # Dots for each point # `group = 1` ensures the line connects the dots  
  geom_point(mapping = aes(x = End, y = Id), size = 3, color = "purple", shape = 21,  fill = NA) +   # Dots for each point 
  labs(title = "Sleep Data", 
       x = "time of day", y = "Id") +
  # coord_polar(start = 0, direction = 1) +
  scale_x_time(
    breaks = scales::breaks_width("2 hours"), 
    labels = scales::time_format("%H:%M")
  )


# maybesort sleep data based on the sd of start and end ???



?scale_x_time


cat("Data analysis completed and results saved!\n")

