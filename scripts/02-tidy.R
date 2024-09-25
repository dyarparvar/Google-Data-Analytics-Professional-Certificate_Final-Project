# script2.R - Data Preprocessing
cat("Starting data preprocessing...\n")

# Assuming my_data was loaded in 01-load.R and is available
# Apply some processing steps (e.g., filtering, cleaning)


# To bring it back you use the readRDS function.
# data_list <- readRDS("rdas/data.rda")





# Step 4: Combine data - different collection times but same attributes
dailyActivity <- rbindlist(data_list[1:2], fill=FALSE)
heartrate <- rbindlist(data_list[3:4], fill=FALSE)
hourlyCalories <- rbindlist(data_list[5:6], fill=FALSE)
hourlyIntensities <- rbindlist(data_list[7:8], fill=FALSE)
hourlySteps <- rbindlist(data_list[9:10], fill=FALSE)
minuteCalories <- rbindlist(data_list[11:12], fill=FALSE)
minuteIntensities <- rbindlist(data_list[13:14], fill=FALSE)
minuteMET <- rbindlist(data_list[15:16], fill=FALSE)
minuteSleep <- rbindlist(data_list[17:18], fill=FALSE)
minuteSteps <- rbindlist(data_list[19:20], fill=FALSE)
weightLog <- rbindlist(data_list[21:22], fill=FALSE)

# check the number of columns
ncol(dailyActivity)

# check the name of columns
colnames(dailyActivity)




library("skimr")
skim_without_charts(dailyActivity)
skim_without_charts(heartrate)
skim_without_charts(hourlyCalories)
skim_without_charts(hourlyIntensities)
skim_without_charts(hourlySteps)
skim_without_charts(minuteCalories)
skim_without_charts(minuteIntensities)
skim_without_charts(minuteMET)
skim_without_charts(minuteSleep)
skim_without_charts(minuteSteps)
skim_without_charts(weightLog)


library(tidyverse)


dailyActivity <- dailyActivity %>% 
  distinct() %>% 
  mutate(daily_total = 100*(VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes+ SedentaryMinutes)/(24*60)) %>% # sum of the time in various activity modes should be equal to 24h*60min/h in a day
  rename(Date = ActivityDate) %>% 
  mutate(Date = mdy(Date))

dailyActivity$daily_total <- round(dailyActivity$daily_total,2) 

dailyActivity <- dailyActivity %>% 
  # filter(daily_total== "100") %>%  # discard the users data that did not use the smart watch the whole time
  filter(Calories != "0") # discard zero calories user data as this represent wrong entry


library(lubridate)

weightLog <- weightLog %>% 
  mutate(Date = parse_date_time(Date, orders = "mdy HMS")) %>% 
  mutate(Date = as.Date(Date))

# adding weight data to daily data 
dailyAll <- left_join(dailyActivity, weightLog, by = c("Id", "Date"))
# skim_without_charts(dailyAll)

dailyAll %>% 
  group_by(Id) %>% 
  drop_na(BMI) %>% 
  summarise(BMI = mean(BMI))


# sleep data
sleep <- minuteSleep %>% 
  rename(Date = date) %>% 
  mutate(Date = parse_date_time(Date, orders = "mdy HMS")) %>% 
  mutate(Time = format(Date, "%H:%M:%S")) %>% 
  mutate(Date = as.Date(Date))


results <- data.frame()
# Loop over each unique date
for (unique_date in unique(sleep$Date)) {
  
  # Filter rows for the current date
  daily_sleep <- sleep %>% filter(Date == unique_date)
  
  # Find min and max time (datetime)
  start <- min(daily_sleep$Time, na.rm = TRUE)
  end <- max(daily_sleep$Time, na.rm = TRUE)
  
  # Calculate duration
  # duration <- difftime(end, start, units = "minutes")
  
  # Store the result
  results <- rbind(results, data.frame(
    Date = unique_date
  ))
}










wideSleep <- sleep %>% 
  filter(value =="1") %>% 
  pivot_wider(
    names_from = Date,
    values_from = Time 
  ) %>% 
  format()
########## duplicates are present! check logId !!!!!!!!


sleepStart <- wideSleep %>% 
  select(-logId, -value) %>% 
  mutate(across(-Id, ~ sapply(., function(x) min(x, na.rm = TRUE))))

sleepEnd <- wideSleep %>% 
  select(-logId, -value) %>% 
  mutate(across(-Id, ~ sapply(., function(x) max(x, na.rm = TRUE))))



sleepData_1 <- sleepStart %>% 
  pivot_longer(
    cols = starts_with("2016"),
    names_to = "Date",
    values_to = "start",
    values_drop_na = TRUE
  ) %>% 
  filter(!apply(., 1, function(x) any(is.infinite(x)))) %>% # remove Inf !!!!!!!
  mutate(dateTime = paste(Date, start, sep = " "))


sleepData_2 <- sleepEnd %>% 
  pivot_longer(
    cols = starts_with("2016"),
    names_to = "Date",
    values_to = "end",
    values_drop_na = TRUE
  )

sleepData <- full_join(sleepData_1, sleepData_2, by = c("Id", "Date")) %>% 
  
  sleepData <- sleepData %>% 
  mutate(sleep_duration = format(end, "%H:%M:%S") - format(start, "%H:%M:%S"))
#check the str of 1, 2 and sleepdata for join correctness




sleepData %>%
  group_by(Id) %>% 
  summarise(Date, start = min(Time))






mutate(sleep_start = min(Time), sleep_end = max(Time)) %>%  
  
  
  
  
  
  
  group_by(Id, Date) %>%
  summarise(sleep_duration = sleep_end - sleep_start)


mutate(SleepDuration = as.numeric(difftime(Time, lag(Time), units = "mins"))) %>%
  ungroup()



group_by(Date) %>% 
  mutate(sleep_start = min(Time), sleep_end = max(Time)) %>%  
  group_by(Id) %>% 
  summarise(sleep_duration = sleep_end - sleep_start)






cat("Data preprocessing completed!\n")
