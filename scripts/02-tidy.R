# Data Preprocessing
cat("Starting data preprocessing...\n")

# Assuming my_data was loaded in 01-load.R and is available
# Apply some processing steps (e.g., filtering, cleaning)


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

# skim data
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


str(dailyActivity)
str(heartrate)
str(hourlyCalories)
str(hourlyIntensities)
str(hourlySteps)
str(minuteCalories)
str(minuteIntensities)
str(minuteMET)
str(minuteSleep)
str(minuteSteps)
str(weightLog)


# check for duplicates
sum(duplicated(dailyActivity))
sum(duplicated(heartrate))
sum(duplicated(hourlyCalories))
sum(duplicated(hourlyIntensities))
sum(duplicated(hourlySteps))
sum(duplicated(minuteCalories))
sum(duplicated(minuteIntensities))
sum(duplicated(minuteMET))
sum(duplicated(minuteSleep))
sum(duplicated(minuteSteps))
sum(duplicated(weightLog))
    
    
dailyActivity <- dailyActivity %>% 
  distinct() %>% 
  drop_na()
heartrate <- heartrate %>% 
  distinct() %>% 
  drop_na()
hourlyCalories <- hourlyCalories %>% 
  distinct() %>% 
  drop_na()
hourlyIntensities <- hourlyIntensities %>% 
  distinct() %>% 
  drop_na()
hourlySteps <- hourlySteps %>% 
  distinct() %>% 
  drop_na()
minuteCalories <- minuteCalories %>% 
  distinct() %>% 
  drop_na()
minuteIntensities <- minuteIntensities %>% 
  distinct() %>% 
  drop_na()
minuteMET <- minuteMET %>% 
  distinct() %>% 
  drop_na()
minuteSleep <- minuteSleep %>% 
  distinct() %>% 
  drop_na()
minuteSteps <- minuteSteps %>% 
  distinct() %>% 
  drop_na()
weightLog <- weightLog %>% 
  distinct() %>% 
  drop_na()




dailyActivity <- dailyActivity %>% 
  distinct() %>% 
  mutate(daily_total = 100*(VeryActiveMinutes+FairlyActiveMinutes+LightlyActiveMinutes+ SedentaryMinutes)/(24*60)) %>% # sum of the time in various activity modes should be equal to 24h*60min/h in a day
  rename(Date = ActivityDate) %>% 
  mutate(Date = mdy(Date))

dailyActivity$daily_total <- round(dailyActivity$daily_total,2) 

dailyActivity <- dailyActivity %>% 
  filter(Calories != "0") # discard zero calories user data as this represent wrong entry
  mutate(daily_usage = case_when(
    daily_total <= quantile(daily_total, 0.25) ~ "Low",
    daily_total <= quantile(daily_total, 0.5) ~ "Medium",
    daily_total <= quantile(daily_total, 0.75) ~ "High",
    TRUE ~ "Very High"
  ))



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





cat("Data preprocessing completed!\n")
