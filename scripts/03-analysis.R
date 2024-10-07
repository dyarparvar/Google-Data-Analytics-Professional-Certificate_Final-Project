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


# to check changes among each individual's sleep behaviour 
# overall sleep occasions for each user

sleepData <- sleepData %>% 
  group_by(logId) %>% 
  mutate(meanS = mean(S), meanE = mean(E), meanD = mean(D)) %>% 
  group_by(Id) %>% 
  summarise(logId = logId, Id = Id, S = S, E = E, D = D, meanStart = mean(meanS), meanEnd = mean(meanE), meanDuration = mean(meanD)) %>% 
  distinct() %>% 
  drop_na() %>% 
  mutate(meanStart = seconds_to_period(meanStart)) %>% 
  mutate(meanEnd = seconds_to_period(meanEnd))  %>% 
  mutate(meanDuration = seconds_to_period(meanDuration*60)) 
  
 

sleepData %>% 
  group_by(Id) %>% 
  filter(n() >= 30) %>% # select users with at least 30 sleep data points
  ungroup() %>% 
  ggplot() +
  geom_violin(mapping = aes(x = S, y = Id, color = "falling asleep"), fill = "dodgerblue3", linewidth = 0) +
  geom_violin(mapping = aes(x = E, y = Id, color = "waking up"), fill = "gold3", linewidth = 0) +
  geom_point(mapping = aes(x = meanStart, y = Id), color = "dodgerblue3", size = 3, shape = 21,  fill = NA) +   # Dots for each point # `group = 1` ensures the line connects the dots  
  geom_point(mapping = aes(x = meanEnd, y = Id), color = "gold3", size = 3, shape = 21,  fill = NA) +
  labs(title = "Sleep behaviour of each individual (n>=30)", 
       x = "time of day (h)", y = "Id", color = "") +
  # coord_polar(start = 0, direction = 1) +
  scale_x_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H")
  ) +
  theme_minimal()

ggsave("all-sleep.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 6, height = 10, dpi = 300, limitsize = FALSE)

sleepData %>% 
  group_by(Id) %>% 
  filter(n() >= 30) %>% # select users with at least 30 sleep data points
  ungroup() %>% 
  mutate(Duration = D/60) %>% 
  filter(Duration <= 3) %>% # nap sleep less than 3 hours duration
  ggplot() +
  geom_point(mapping = aes(x = S, y = E, color = Duration), shape = 3,  fill = NA) +
  geom_point(mapping = aes(x = meanStart, y = meanEnd), color = "black", shape = 21,  fill = NA ) + 
  facet_wrap(~Id) +
  labs(title = "Nap behaviour of each individual (duration <= 3h) (n>=30)", 
       x = "falling asleep (h)", y = "waking up (h)", color = "nap duration (h)") +
  scale_x_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H")
  ) +
  scale_y_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H")
  ) +
  scale_color_gradientn(colors = brewer.pal(9, "YlOrBr")[5:9],
                        values = scales::rescale(c(0, 3)),
                        breaks = c(0, 1, 2, 3),
                        labels = c("0", "1", "2", "3")
  ) + # min, max & 8 hours recommended 
  theme_minimal() +
  theme( # modify legend after theme_minimal() otherwise it will overwrite it!
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),  
    legend.text = element_text(size = 10)         
  ) 
ggsave("nap-sleep-v2.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 10, dpi = 300, limitsize = FALSE)




sleepData %>% 
  group_by(Id) %>% 
  filter(n() >= 30) %>% # select users with at least 30 sleep data points
  ungroup() %>% 
  mutate(Duration = D/60) %>% 
  filter(Duration > 3) %>% # long sleep more than 3 hours duration
  ggplot() +
  geom_point(mapping = aes(x = S, y = E, color = Duration), shape = 3,  fill = NA) +
  geom_point(mapping = aes(x = meanStart, y = meanEnd), color = "black", shape = 21,  fill = NA ) + 
  facet_wrap(~Id) +
  labs(title = "Long sleep behaviour of each individual (duration > 3h)", 
       x = "falling asleep (h)", y = "waking up (h)", color = "sleep duration (h)") +
  scale_x_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H")
  ) +
  scale_y_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H")
  ) +
  scale_color_gradientn(colors = brewer.pal(9, "YlOrBr")[5:9],
                        values = scales::rescale(c(0, 8, 15)),
                        breaks = c(0, 8),
                        labels = c("", "recommended 8 hours of sleep")
  ) + # min, max & 8 hours recommended 
  theme_minimal() +
  theme( # modify legend after theme_minimal() otherwise it will overwrite it!
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),  
    legend.text = element_text(size = 10)         
  ) 
ggsave("long-sleep-v2.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 10, dpi = 300, limitsize = FALSE)


sleepData %>% 
  filter(D > 3*60) %>%
  group_by(Id) %>% 
  filter(n() >= 30) %>% # select users with at least 30 sleep data points
  ungroup() %>% 
  ggplot() +
  geom_violin(mapping = aes(x = S, y = Id, color = "falling asleep"), fill = "dodgerblue3", linewidth = 0) +
  geom_violin(mapping = aes(x = E, y = Id, color = "waking up"), fill = "gold3", linewidth = 0) +
  geom_point(mapping = aes(x = meanStart, y = Id), size = 3, shape = 21,  fill = "dodgerblue3") +   # Dots for each point # `group = 1` ensures the line connects the dots  
  geom_point(mapping = aes(x = meanEnd, y = Id), size = 3, shape = 21,  fill = "gold3") +
  labs(title = "Long sleep behaviour of each individual (duration > 3h) (n>=30)", 
       x = "time of day", y = "Id", color = "") +
  scale_x_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H")
  ) +
  theme_minimal()

ggsave("long-sleep-v1.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 6, dpi = 300, limitsize = FALSE)


sleepData %>% 
  filter(D <= 3*60) %>% 
  group_by(Id) %>% 
  filter(n() >= 30) %>% # select users with at least 30 sleep data points
  ungroup() %>% 
  ggplot() +
  geom_violin(mapping = aes(x = S, y = Id, color = "falling asleep"), fill = "dodgerblue3", linewidth = 0) +
  geom_violin(mapping = aes(x = E, y = Id, color = "waking up"), fill = "gold3", linewidth = 0) +
  geom_point(mapping = aes(x = meanStart, y = Id), size = 3, shape = 21,  fill = "dodgerblue3") +   # Dots for each point # `group = 1` ensures the line connects the dots  
  geom_point(mapping = aes(x = meanEnd, y = Id), size = 3, shape = 21,  fill = "gold3") +
  labs(title = "Nap behaviour of each individual (duration < 3h) (n>=30)", 
       x = "time of day", y = "Id") +
  scale_x_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H")
  ) +
  scale_color_manual(
    name = "Sleep Time", 
    values = c("Start" = "dodgerblue3", "End" = "gold3")
  )

ggsave("nap-v1.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 6, height = 10, dpi = 300, limitsize = FALSE)

# each individual sleep occasion for each user

sleepData %>% 
  filter(D > 3*60) %>% 
  group_by(Id) %>% 
  filter(n() >= 30) %>% # select users with at least 30 sleep data points
  ungroup() %>% 
  ggplot() +
  geom_point(mapping = aes(x = S, y = Id, color = "falling asleep"), color = "dodgerblue3", size = 3, shape = 21,  fill = NA) +   # Dots for each point # `group = 1` ensures the line connects the dots  
  geom_point(mapping = aes(x = E, y = Id, color = "waking up"), color = "gold3", size = 3, shape = 21,  fill = NA) +   # Dots for each point 
  labs(title = "Long sleep instances for each individual (duration > 3h) (n>=30)", 
       x = "time of day (h)", y = "Id") +
  scale_x_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H:%M")
    ) +
  theme_minimal()


ggsave("long-sleep-points.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 6, dpi = 300, limitsize = FALSE)

sleepData %>% 
  filter(D <= 3*60) %>% 
  ggplot() +
  geom_point(mapping = aes(x = S, y = Id, color = "falling asleep"), color = "dodgerblue3", size = 3, shape = 21,  fill = NA) +   # Dots for each point # `group = 1` ensures the line connects the dots  
  geom_point(mapping = aes(x = E, y = Id, color = "waking up"), color = "gold3", size = 3, shape = 21,  fill = NA) +   # Dots for each point 
  labs(title = "Nap instances for each individual (duration <= 3h) (n>=30)", 
       x = "time of day (h)", y = "Id") +
  scale_x_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H:%M")
  ) + 
  theme_minimal()


ggsave("nap-points.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 6, dpi = 300, limitsize = FALSE)





# to check differences among the population


sleepData %>% 
  ggplot() +
  geom_violin(mapping = aes(x = meanStart, y = "falling asleep" ), fill = "dodgerblue3", linewidth = 0) +
  geom_violin(mapping = aes(x = meanEnd, y = "waking up"), fill = "gold3", linewidth = 0) +
  geom_violin(mapping = aes(x = meanDuration, y = "duration"), fill = "lavenderblush4", linewidth = 0) +
  labs(title = "sleep schedule", x = "time (h)", y = "") +
  scale_x_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H")
  ) +
  theme_minimal()


ggsave("sleep-population.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 6, height = 6, dpi = 300, limitsize = FALSE)


sleepData %>%
  filter(D > 3*60) %>% 
  group_by(Id) %>%
  filter(n() >= 30) %>% # select users with at least 30 sleep data points
  ungroup() %>%
  ggplot() +
  geom_violin(mapping = aes(x = meanStart, y = "falling asleep" ), fill = "dodgerblue3", linewidth = 0) +
  geom_violin(mapping = aes(x = meanEnd, y = "waking up"), fill = "gold3", linewidth = 0) +
  geom_violin(mapping = aes(x = meanDuration, y = "sleep duration"), fill = "lavenderblush4", linewidth = 0) +
  labs(title = "long sleep schedule (duration > 3h) (n>=30)", x = "time (h)", y = "") +
  scale_x_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H")
  ) +
  theme_minimal()


ggsave("long-sleep-population.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 6, height = 6, dpi = 300, limitsize = FALSE)

sleepData %>%
  filter(D <= 3*60) %>% 
  # group_by(Id) %>%
  # filter(n() >= 30) %>% # select users with at least 30 sleep data points
  # ungroup() %>% 
  ggplot() +
  geom_violin(mapping = aes(x = meanStart, y = "falling asleep" ), fill = "dodgerblue3", linewidth = 0) +
  geom_violin(mapping = aes(x = meanEnd, y = "waking up"), fill = "gold3", linewidth = 0) +
  geom_violin(mapping = aes(x = meanDuration, y = "sleep duration"), fill = "lavenderblush4", linewidth = 0) +
  
  labs(title = "nap schedule (duration <= 3h) !(n>=30)", x = "time (hour)", y = "") +
  scale_x_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H")
  ) +
  theme_minimal()



ggsave("nap-population.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 6, dpi = 300, limitsize = FALSE)


# Line connecting Start and End
# geom_segment(aes(x = meanStart, xend = meanEnd, y = Id, yend = Id), color = "lavenderblush4", size = 1) +


################### Steps

dailystepsData <- stepsData %>% 
  group_by(Id, Date) %>% 
  summarise(Id = Id, Date = Date, totalSteps = sum(StepTotal)) %>% 
  mutate(Id = format(Id, scientific = FALSE)) %>% 
  distinct() %>%
  drop_na() %>% 
  arrange(Id)

dailystepsData %>% 
  group_by(Id) %>%
  filter(n() >= 30) %>% # select users with at least 30 sleep data points
  ungroup() %>%
  ggplot() +
  geom_violin(mapping = aes(x = totalSteps , y = Id ), fill = "darkorange1", linewidth = 0) +
  labs(title = "daily steps (n>=30)", x = "total steps", y = "Id") +
  theme_minimal()
  
ggsave("steps-individual.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 6, height = 10, dpi = 300, limitsize = FALSE)

dailystepsData %>% 
  group_by(Id) %>%
  filter(n() >= 30) %>% # select users with at least 30 sleep data points
  ungroup() %>%
  ggplot() +
  geom_violin(mapping = aes(x = totalSteps , y = "" ), fill = "darkorange1", linewidth = 0) +
  labs(title = "daily steps (n>=30)", x = "total steps", y = "")  +
  theme_minimal()



hourlystepsData <- stepsData %>% 
  group_by(Id, Time) %>% 
  summarise(Id = Id, Time = Time, meanSteps = mean(StepTotal), totalSteps = sum(StepTotal)) %>% 
  mutate(Id = format(Id, scientific = FALSE)) %>% 
  mutate(Time = hms::as_hms(Time)) %>% 
  distinct() %>%
  drop_na() %>% 
  arrange(Id)

hourlystepsData %>% 
  ggplot() +
  geom_point(mapping = aes(x = Time , y = meanSteps), color = "darkorange1", shape = 21, fill = NA, size = 1) +
  labs(title = "hourly steps", x = "time of day (h)", y = "mean steps") +
  facet_wrap(~Id) +
  scale_x_time(
    breaks = scales::breaks_width("6 hours"), 
    labels = scales::time_format("%H")
  ) +
  theme_minimal()

ggsave("meansteps-individual-hourly.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 6, dpi = 300, limitsize = FALSE)

hourlystepsData %>% 
  ggplot() +
  geom_point(mapping = aes(x = Time , y = totalSteps), color = "darkorange1", shape = 21, fill = NA, size = 1) +
  labs(title = "hourly steps", x = "time of day (h)", y = "total steps") +
  facet_wrap(~Id) +
  scale_x_time(
    breaks = scales::breaks_width("6 hours"), 
    labels = scales::time_format("%H")
  ) +
  theme_minimal()

ggsave("totalsteps-individual-hourly.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 6, dpi = 300, limitsize = FALSE)


hourlystepsData %>% 
  ggplot() +
  geom_point(mapping = aes(x = Time , y = meanSteps), color = "darkorange1", shape = 2, fill = NA, size = 1) +
  geom_smooth(mapping = aes(x = Time, y = meanSteps), method = "gam", se = TRUE, color = "lavenderblush4", linewidth = 0.5) +
  labs(title = "hourly steps", x = "time of day (h)", y = "mean steps", color = "") +
  scale_x_time(
    breaks = scales::breaks_width("3 hours"), 
    labels = scales::time_format("%H")
  ) +
  theme_minimal() +
  theme(
    legend.position = "none"  # Remove the legend completely
  )

ggsave("steps-all-hourly.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 6, dpi = 300, limitsize = FALSE)




######## Steps & Sleep
joinData <- stepsData %>% 
  mutate(Id = format(Id, scientsific = FALSE)) %>% 
  group_by(Id, Date) %>% 
  summarise(sumSteps = sum(StepTotal, na.rm = TRUE)) %>% 
  ungroup() %>% 
  group_by(Id) %>% 
  summarise(meanDailySteps = mean(sumSteps, na.rm = TRUE)) %>% 
  left_join(sleepData, by = "Id") 

# compare long sleep occasions of each individual with their mean steps
joinData %>% 
  filter(D > 3*60) %>%
  arrange(Id) %>% 
  group_by(Id) %>%
  filter(n() >= 30) %>% # select users with at least 30 sleep data points
  ungroup() %>%
  group_by(Id) %>% 
  summarise(mean_S = mean(S, na.rm = TRUE),
            mean_E = mean(E, na.rm = TRUE),
            mean_D = mean(D, na.rm = TRUE),
            sd_S = sd(S, na.rm = TRUE),
            sd_E = sd(E, na.rm = TRUE),
            sd_D = sd(D, na.rm = TRUE),
            n = n(), 
            meanDailySteps = meanDailySteps,
            S, E, D) %>%
  mutate(se_S = sd_S / sqrt(n()),
         se_E = sd_E / sqrt(n()),
         se_D = sd_D / sqrt(n())) %>%   # Calculate standard error
  ggplot() +
  geom_point(mapping = aes(x = S, y = E, color = meanDailySteps), shape = 3,  fill = NA) +   # Dots for each point # `group = 1` ensures the line connects the dots  
    geom_errorbar(mapping = aes(x = mean_S, xmin = mean_S - se_S, xmax = mean_S + se_S, y = mean_E), width = 0.1, color = "black") +
  geom_errorbar(mapping = aes(x = mean_S, y = mean_E, ymin = mean_E - se_E, ymax = mean_E + se_E), width = 0.1, color = "black") +
    # geom_errorbar(aes(ymin = meanDuration - se_D, ymax = meanDuration + se_D), 
    #               width = 0.1, color = "lavenderblush4")   # Error bars for y
  facet_wrap(~Id, nrow = 4) +
  labs(title = "Long sleep instances for each individual (duration > 3h) and their mean daily steps (n>=30)", 
       x = "falling asleep (h)", y = "waking up (h)", color = "mean daily steps") +
  scale_x_time(
    breaks = scales::breaks_width("6 hours"), 
    labels = scales::time_format("%H:%M")
  ) +
  scale_y_time(
    breaks = scales::breaks_width("6 hours"), 
    labels = scales::time_format("%H:%M")
  )  +
  scale_color_gradient(low = "cyan2", high = "darkorange1", 
                       limits = c(0, 13000),  # Specify range of data
                       breaks = c(0, 7000, 13000),
                       labels = c("0", "recommended 7k steps per day", "13k")
  ) +
  theme_minimal() +
  theme( # modify legend after theme_minimal() otherwise it will overwrite it!
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),  
    legend.text = element_text(size = 10)         
  ) 
  
    


ggsave("sleep-steps-individual.png", plot = last_plot(), path = output_dir,
       scale = 1, width = 10, height = 10, dpi = 300, limitsize = FALSE)


cat("Exploraatory data analysis completed and results saved!\n")

