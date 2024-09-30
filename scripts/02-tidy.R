# Data Processing
cat("Starting data processing...\n")

# Assuming my_data was loaded in 01-load.R and is available
# Apply some processing steps (e.g., filtering, cleaning)


# Step 4: Combine data - different collection times but same attributes
dailyActivity <- rbindlist(data_list[1:2], fill=FALSE)
# heartrate <- rbindlist(data_list[3:4], fill=FALSE) # not enough sample size
hourlyCalories <- rbindlist(data_list[5:6], fill=FALSE)
# hourlyIntensities <- rbindlist(data_list[7:8], fill=FALSE) # no need to this much fine resolution for this business task
hourlySteps <- rbindlist(data_list[9:10], fill=FALSE)
# minuteCalories <- rbindlist(data_list[11:12], fill=FALSE) # no need to this much fine resolution for this business task - can be used to verify the hourlyCalories data
# minuteIntensities <- rbindlist(data_list[13:14], fill=FALSE)
# minuteMET <- rbindlist(data_list[15:16], fill=FALSE) # not enough Meta data
minuteSleep <- rbindlist(data_list[17:18], fill=FALSE) 
# minuteSteps <- rbindlist(data_list[19:20], fill=FALSE) # no need to this much fine resolution for this business task - can be used to verify the hourlySteps data
# weightLog <- rbindlist(data_list[21:22], fill=FALSE) # not enough sample size


# # skim data
# skim_without_charts(dailyActivity)
# skim_without_charts(hourlyCalories)
# skim_without_charts(hourlySteps)
# skim_without_charts(minuteSleep)

 
# # check for duplicates & remove them
sum(duplicated(dailyActivity))
sum(duplicated(hourlyCalories))
sum(duplicated(hourlySteps))
sum(duplicated(minuteSleep))

dailyActivity <- dailyActivity %>%
  distinct() %>%
  drop_na()
hourlyCalories <- hourlyCalories %>%
  distinct() %>%
  drop_na()
hourlySteps <- hourlySteps %>%
  distinct() %>%
  drop_na()
minuteSleep <- minuteSleep %>%
  distinct() %>%
  drop_na()



# Activity & Usage Data

activityData <- dailyActivity %>% 
  filter(Calories != "0") %>%  # discard zero calories user data as this represent wrong entry
  mutate(daily_usage = 100*(VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes+ SedentaryMinutes)/(24*60)) %>% # sum of the time in various activity modes will show how much the users used their device
  filter(daily_usage >= 10) %>% 
  rename(Date = ActivityDate) %>% 
  mutate(Date = mdy(Date)) 


activityData$daily_usage <- round(activityData$daily_usage, 2) 


# Calories Data
caloriesData <- hourlyCalories %>%
  rename(DateTime = ActivityHour) %>%
  mutate(DateTime = parse_date_time(DateTime, orders = "mdy IMS p")) %>%
  mutate(Time = format(DateTime, "%H:%M:%S")) %>% # It's crucial to take into account the AM/PM format of the initial data and convert it to 24-hour format from the very first step.
  mutate(Date = as.Date(DateTime))

# calculate daily calories from the hourly information and compare it with daily calories from dailyActivity
dailyCalories <- caloriesData %>%
  group_by(Date, Id) %>% 
  summarise(DailyCalories = sum(Calories), .groups = 'drop')

activityData <- left_join(activityData, dailyCalories, by = c("Date" = "Date", "Id" = "Id"))

filter_out_Data <- activityData %>% 
  filter(abs(Calories - DailyCalories) >= "100") # see how many rows have calories difference higher than 100 (I'm assuming 100 cal is negligible. 5% margin of error for average 2000cal/day)

activityData <- activityData %>% 
  filter(abs(Calories - DailyCalories) < "100") # keep the data that has calories difference less than 100


# Sleep Data
sleepData <- minuteSleep %>%
  rename(DateTime = date) %>%
  mutate(DateTime = parse_date_time(DateTime, orders = "mdy IMS p")) %>% # It's crucial to take into account the AM/PM format of the initial data and convert it to 24-hour format from the very first step.
  mutate(Time = format(DateTime, "%H:%M:%S")) %>% 
  mutate(Date = as.Date(DateTime)) %>% 
  group_by(logId, Id) %>% # grouping by logId makes it possible to find the time they go to bed and the time they wake up. If group-by Date, then it won't be possible to find this info. becasue usually the Date changes during sleeping overnight.
  arrange(DateTime) %>% 
  mutate(Start = first(DateTime),  # Get first timestamp for each group
         End = last(DateTime), 
         Duration = if_else(End >= Start),
                            as.numeric(difftime(End, Start, units = "mins")),
                            as.numeric(difftime(End + days(1), Start, units = "mins")))
  )


# Steps Data
stepsData <- hourlySteps %>%
  rename(DateTime = ActivityHour) %>%
  mutate(DateTime = parse_date_time(DateTime, orders = "mdy IMS p")) %>%
  mutate(Time = format(DateTime, "%H:%M:%S")) %>% # It's crucial to take into account the AM/PM format of the initial data and convert it to 24-hour format from the very first step.
  mutate(Date = as.Date(DateTime))

sleepData <- sleepData %>% 
  mutate(nS = as_hms(Start), nE = as_hms(End)) %>%
  mutate(nS = as.numeric(nS), nE = as.numeric(nE)) %>%
  group_by(Id) %>% 
  mutate(meanStart = mean(nS), meanEnd = mean(nE)) %>% 
  mutate(meanStart = seconds_to_period(nS), meanEnd = seconds_to_period(nE)) %>% 
  mutate(nD = nE - nS) %>% 
  mutate(meanDuration = seconds_to_period(nD))


  

cat("Data preprocessing completed!\n")
