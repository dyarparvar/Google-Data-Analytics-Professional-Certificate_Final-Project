# Data Analysis
cat("Performing exploraatory data analysis...\n")



# Save the summary to the output folder if required
# summary_stats <- summary(dailyUsage)
# write.csv(summary_stats, file.path(output_dir, "dailyUsage_stats.csv"))

# Save each graph
# add the following line before each graph
# png(file.path(output_dir, "daily_usage.png"), bg = "transparent", width = 800, height = 600) # Set width and height as needed
# add the following line at the end on each graph
# dev.off() 


################### Usage

dailyUsage <- activityData %>% 
  group_by(Id) %>% 
  summarise(mean_usage = mean(Usage), sd_usage = sd(Usage)) %>% 
  drop_na()

mean <- mean(dailyUsage$mean_usage)
sd <- sd(dailyUsage$sd_usage)

dailyUsage <- dailyUsage %>% 
  mutate(Usage = case_when(
    mean_usage == "100" & sd_usage == "0" ~ "whole-time",
    mean_usage < (mean - sd) ~ "low",
    mean_usage >= (mean - sd) & mean_usage <= (mean + sd) ~ "medium",
    mean_usage > (mean + sd) ~ "high"
  ))

desired_order <- c("low", "medium", "high", "whole-time")  # Specify the order
dailyUsage <- dailyUsage %>%
  mutate(Usage = factor(Usage, levels = desired_order)) %>%
  arrange(Usage)





# distribution of daily usage - not grouped by Id
activityData %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = Usage), stat="bin", binwidth = 5) +
  labs(title = "distribution of daily usage - not grouped by Id", x = "usage", y = "frequency") +
  theme_minimal()

# distribution of daily usage - grouped by Id
dailyUsage %>% 
  ggplot() +
  geom_histogram(mapping = aes(x = mean_usage), stat="bin", binwidth = 5) +
  labs(title = "distribution of daily usage - grouped by Id", x = "usage", y = "frequency") +
  theme_minimal()

# the difference between above graphs shows that we have users that don't have consistent behaviour!!



png(file.path(output_dir, "daily_usage.png"), bg = "transparent", width = 1000, height = 400) # Set width and height as needed  

dailyUsage %>% 
  ggplot() +
  geom_bar(mapping = aes(x = Usage, fill = Usage), width = 10/13) +
  labs(title = "daily usage", x = "daily usage", y = "number of users")


dev.off() 



################### Sleep

# exploratory


sleepData <- sleepData %>% 
  mutate(S = as_hms(Start), E = as_hms(End)) %>%
  mutate(S = as.numeric(S), E = as.numeric(E)) %>% 
  mutate(D = Duration) %>% 
  mutate(Id = format(Id, scientific = FALSE)) %>% 
  arrange(Id)


# to check changes among each individual's behaviour 
# overall sleep occasions for each user

sleepData %>% 
  group_by(logId) %>% 
  mutate(meanS = mean(S), meanE = mean(E), meanD = mean(D)) %>% 
  group_by(Id) %>% 
  mutate(meanStart = mean(meanS), meanEnd = mean(meanE), meanDuration = mean(meanD)) %>% 
  mutate(meanStart = seconds_to_period(meanStart)) %>% 
  mutate(meanEnd = seconds_to_period(meanEnd))  %>% 
  mutate(meanDuration = seconds_to_period(meanDuration*60))  %>% 
  ggplot() +
  geom_violin(mapping = aes(x = S, y = Id, color = "Start"), fill = "blue") +
  geom_violin(mapping = aes(x = E, y = Id, color = "End"), fill = "orange") +
  geom_point(mapping = aes(x = meanStart, y = Id, color = "Start"), size = 3, shape = 21,  fill = NA) +   # Dots for each point # `group = 1` ensures the line connects the dots  
  geom_point(mapping = aes(x = meanEnd, y = Id, color = "End"), size = 3, shape = 21,  fill = NA) +
  labs(title = "Sleep behaviour of each individual", 
       x = "time of day", y = "Id") +
  # coord_polar(start = 0, direction = 1) +
  scale_x_time(
    breaks = scales::breaks_width("2 hours"), 
    labels = scales::time_format("%H")
  ) +
  scale_color_manual(
    name = "Sleep Time", 
    values = c("Start" = "blue", "End" = "orange")
  )

ggsave("all-sleep.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 6, height = 10, dpi = 300, limitsize = FALSE)



sleepData %>% 
  filter(Duration > 3*60) %>% 
  group_by(logId) %>% 
  mutate(meanS = mean(S), meanE = mean(E), meanD = mean(D)) %>% 
  group_by(Id) %>% 
  mutate(meanStart = mean(meanS), meanEnd = mean(meanE), meanDuration = mean(meanD)) %>% 
  mutate(meanStart = seconds_to_period(meanStart)) %>% 
  mutate(meanEnd = seconds_to_period(meanEnd))  %>% 
  mutate(meanDuration = seconds_to_period(meanDuration*60))  %>% 
  ggplot() +
  geom_violin(mapping = aes(x = S, y = Id, color = "Start"), fill = "blue") +
  geom_violin(mapping = aes(x = E, y = Id, color = "End"), fill = "orange") +
  geom_point(mapping = aes(x = meanStart, y = Id, color = "Start"), size = 3, shape = 21,  fill = NA) +   # Dots for each point # `group = 1` ensures the line connects the dots  
  geom_point(mapping = aes(x = meanEnd, y = Id, color = "End"), size = 3, shape = 21,  fill = NA) +
  labs(title = "Long Sleep behaviour of each individual (duration > 3h)", 
       x = "time of day", y = "Id") +
  scale_x_time(
    breaks = scales::breaks_width("2 hours"), 
    labels = scales::time_format("%H")
  ) +
  scale_color_manual(
    name = "Sleep Time", 
    values = c("Start" = "blue", "End" = "orange")
  )


ggsave("long-sleep.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 6, dpi = 300, limitsize = FALSE)


sleepData %>% 
  filter(Duration <= 3*60) %>% 
  group_by(logId) %>% 
  mutate(meanS = mean(S), meanE = mean(E), meanD = mean(D)) %>% 
  group_by(Id) %>% 
  mutate(meanStart = mean(meanS), meanEnd = mean(meanE), meanDuration = mean(meanD)) %>% 
  mutate(meanStart = seconds_to_period(meanStart)) %>% 
  mutate(meanEnd = seconds_to_period(meanEnd))  %>% 
  mutate(meanDuration = seconds_to_period(meanDuration*60))  %>% 
  ggplot() +
  geom_violin(mapping = aes(x = S, y = Id, color = "Start"), fill = "blue") +
  geom_violin(mapping = aes(x = E, y = Id, color = "End"), fill = "orange") +
  geom_point(mapping = aes(x = meanStart, y = Id, color = "Start"), size = 3, shape = 21,  fill = NA) +   # Dots for each point # `group = 1` ensures the line connects the dots  
  geom_point(mapping = aes(x = meanEnd, y = Id, color = "End"), size = 3, shape = 21,  fill = NA) +
  labs(title = "Nap behaviour of each individual (duration < 3h)", 
       x = "time of day", y = "Id") +
  scale_x_time(
    breaks = scales::breaks_width("2 hours"), 
    labels = scales::time_format("%H")
  ) +
  scale_color_manual(
    name = "Sleep Time", 
    values = c("Start" = "blue", "End" = "orange")
  )

ggsave("nap.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 6, height = 10, dpi = 300, limitsize = FALSE)

# each individual sleep occasion for each user

sleepData %>% 
  filter(Duration > 3*60) %>% 
  group_by(logId) %>% 
  group_by(Id) %>% 
  ggplot() +
  geom_point(mapping = aes(x = S, y = Id, color = "Start"), size = 3, shape = 21,  fill = NA) +   # Dots for each point # `group = 1` ensures the line connects the dots  
  geom_point(mapping = aes(x = E, y = Id, color = "End"), size = 3, shape = 21,  fill = NA) +   # Dots for each point 
  labs(title = "Long Sleep ", 
       x = "time of day", y = "Id") +
  scale_x_time(
    breaks = scales::breaks_width("2 hours"), 
    labels = scales::time_format("%H:%M")
    ) +
  scale_color_manual(
    name = "Sleep Time", 
    values = c("Start" = "blue", "End" = "orange")
  )


ggsave("long-sleep-points.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 6, dpi = 300, limitsize = FALSE)

sleepData %>% 
  filter(Duration <= 3*60) %>% 
  group_by(logId) %>% 
  group_by(Id) %>% 
  ggplot() +
  geom_point(mapping = aes(x = S, y = Id, color = "Start"), size = 3, shape = 21,  fill = NA) +   # Dots for each point # `group = 1` ensures the line connects the dots  
  geom_point(mapping = aes(x = E, y = Id, color = "End"), size = 3, shape = 21,  fill = NA) +   # Dots for each point 
  labs(title = "Nap Data - sleep duration less than 3 hours", 
       x = "time of day", y = "Id") +
  scale_x_time(
    breaks = scales::breaks_width("2 hours"), 
    labels = scales::time_format("%H:%M")
  ) +
  scale_color_manual(
    name = "Sleep Time", 
    values = c("Start" = "blue", "End" = "orange")
  )


ggsave("nap-points.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 6, dpi = 300, limitsize = FALSE)





# to check differences among the population

longsleepMean <- sleepData %>%
  filter(Duration > 3*60) %>% 
  group_by(logId) %>% 
  mutate(meanS = mean(S), meanE = mean(E), meanD = mean(D)) %>% 
  group_by(Id) %>% 
  mutate(meanStart = mean(meanS), meanEnd = mean(meanE), meanDuration = mean(meanD)) %>% 
  mutate(meanStart = seconds_to_period(meanStart)) %>% 
  mutate(meanEnd = seconds_to_period(meanEnd))  %>% 
  mutate(meanDuration = seconds_to_period(meanDuration*60))

longsleepMean %>% 
  ggplot() +
  geom_violin(mapping = aes(x = meanStart, y = "" ), fill = "blue") +
  geom_violin(mapping = aes(x = meanEnd, y = ""), fill = "orange") +
  geom_violin(mapping = aes(x = meanDuration, y = ""), fill = "grey") +
  labs(title = "sleep schedule", x = "time (hour)", y = "") +
  scale_x_time(
    breaks = scales::breaks_width("2 hours"), 
    labels = scales::time_format("%H")
  ) +
  theme_minimal()


ggsave("long-sleep-population.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 6, height = 6, dpi = 300, limitsize = FALSE)

napMean <- sleepData %>%
  filter(Duration <= 3*60) %>% 
  group_by(logId) %>% 
  mutate(meanS = mean(S), meanE = mean(E), meanD = mean(D)) %>% 
  group_by(Id) %>% 
  mutate(meanStart = mean(meanS), meanEnd = mean(meanE), meanDuration = mean(meanD)) %>% 
  mutate(meanStart = seconds_to_period(meanStart)) %>% 
  mutate(meanEnd = seconds_to_period(meanEnd))  %>% 
  mutate(meanDuration = seconds_to_period(meanDuration*60))

napMean %>% 
  ggplot() +
  geom_violin(mapping = aes(x = meanEnd, y = ""), fill = "orange") +
  geom_violin(mapping = aes(x = meanDuration, y = ""), fill = "grey") +
  geom_violin(mapping = aes(x = meanStart, y = "" ), fill = "blue") +
  labs(title = "sleep schedule", x = "time (hour)", y = "") +
  scale_x_time(
    breaks = scales::breaks_width("2 hours"), 
    labels = scales::time_format("%H")
  ) +
  theme_minimal()



ggsave("nap-population.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 6, dpi = 300, limitsize = FALSE)


# Line connecting Start and End
# geom_segment(aes(x = meanStart, xend = meanEnd, y = Id, yend = Id), color = "grey", size = 1) +


cat("Exploraatory data analysis completed and results saved!\n")

