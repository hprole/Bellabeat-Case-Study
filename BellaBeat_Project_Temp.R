# BellaBeat Project

library(tidyverse)
library(ggplot2)
library(tidymodels)
library(plotly)

library(plotly)


# Read in data

df <- read.csv("https://cdn.rawgit.com/plotly/datasets/master/GanttChart-updated.csv", stringsAsFactors = F)

bb_colors <- c("#FD8E76", "#004aad", "#008080", "#7D0080", "#767676","#FFE3ED")


# create GANTT Chart with Plotly
# Gantt data formatted in table

gantt_dates <- read_csv("Bellabeat_Gantt.csv") %>%
  rename(Start = 'Start Date',
         End = "End Date") %>%
  mutate(Duration = as.numeric(End - Start))

# Add each trace for the GANTT chart

plot_gantt <- plot_ly()

for(i in 1:(nrow(gantt_dates))){
  
  plot_gantt <- add_trace(plot_gantt,
                          x = c(gantt_dates$Start[i], gantt_dates$Start[i] + gantt_dates$Duration[i]),  # x0, x1
                          y = c(i, i),  # y0, y1
                          mode = "lines",
                          line = list(color = bb_colors[1], width = 15),
                          showlegend = F,
                          evaluate = T  # needed to avoid lazy loading
  )
}

# Y-Axis reverse order and add tick labels using NAMES

plot_gantt %>%
  layout(plot_gantt,
        xaxis = list(showgrid = T),
        yaxis = list(showgrid = T, autorange="reversed", 
                     #categoryorder = "array", categoryarray = unique(gantt_dates$Task),
                     tickmode = "array", tickvals = 1:(nrow(gantt_dates)), ticktext = unique(gantt_dates$Task),
                     domain = c(0, 0.9)),
        plot_bgcolor = bb_colors[6],  # Chart area color
        paper_bgcolor = bb_colors[6])

# Import daily activity
# Reformat DATE
# Create variables for total minutes and total activity

daily_activity_wide <- read_csv("dailyActivity_merged.csv") %>%
  rowwise() %>%
  mutate(ActivityDate = mdy(ActivityDate),
         TotalMinutes = sum(LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes, SedentaryMinutes),
         TotalActiveMinutes = sum(LightlyActiveMinutes, FairlyActiveMinutes, VeryActiveMinutes)) %>%
  ungroup() #%>% 
  #summarise(Min = min(ActivityDate),
  #          Max = max(ActivityDate)) # Sample day count

# Count number of active days

daily_act_count <- daily_activity_wide %>%
  group_by(Id) %>% 
  count()

# Reshape data for long format

daily_activity <- daily_activity_wide %>%
  pivot_longer(names_to = "Metric", values_to = "Value", -c(Id, ActivityDate)) %>%
  mutate(Activity = "Active") %>%
  rename(Date = ActivityDate) %>%
  dplyr::select(Id, Date, Activity, Metric, Value)

# Import sleep data and reformat

sleep_day_wide <- read.csv("sleepDay_merged.csv") %>%
  mutate(SleepDay = mdy_hms(SleepDay)) %>%
  rename(Date = SleepDay)

# Count sleep days

sleep_day_count <- sleep_day_wide %>%
  group_by(Id) %>% 
  count()

# Count weekdays for all measurements (Activity, Sleep)

weekday_count <- daily_activity_wide %>%
  dplyr::select(Id, ActivityDate) %>%
  rename(Date = ActivityDate) %>%
  mutate(Activity = 1) %>%
  full_join(sleep_day_wide[1:3], by = c("Id", "Date")) %>%
  rename(Sleep = TotalSleepRecords) %>%
  mutate(Date = ymd(Date),
         DayNum = wday(Date),
         Activity = ifelse(is.na(Activity), 0, 1),
         Sleep = ifelse(is.na(Sleep), 0, 1)) %>%
  group_by(DayNum) %>%
  summarize(Activity = sum(Activity),
            Sleep = sum(Sleep)) %>%
  mutate(Weekday = wday(DayNum, label = TRUE)) %>%
  dplyr::select(Weekday, Activity, Sleep)

# Create plot for count by weekday

plot_weekday <- plot_ly(weekday_count) %>%
  add_bars(x=~Weekday, y=~Activity, text=~Activity, name = "Active", 
           marker = list(color = bb_colors[2], opacity = 0.6)) %>%
  add_bars(x=~Weekday, y=~Sleep, text=~Sleep, textposition = "above", name = "Sleep", 
           marker = list(color = bb_colors[3], opacity = 0.6)) %>%
  layout(yaxis = list(title = "Entries"),
         legend = list(x = 0, y = 100, orientation = "h"), 
         paper_bgcolor = bb_colors[6], 
         plot_bgcolor = bb_colors[6])

# Shape data for weekday mean by activity

weekday_mean <- daily_activity_wide %>%
  dplyr::select(Id, ActivityDate, TotalSteps) %>%
  rename(Date = ActivityDate,
         Steps = TotalSteps) %>%
  full_join(sleep_day_wide[c(1:2,4)], by = c("Id", "Date")) %>%
  rename(Sleep = TotalMinutesAsleep) %>%
  mutate(Date = ymd(Date),
         DayNum = wday(Date)) %>%
  group_by(DayNum) %>%
  summarize(Steps = round(mean(Steps, na.rm = TRUE),0),
            Sleep = round(mean(Sleep, na.rm = TRUE)/60,1)) %>%
  mutate(Weekday = wday(DayNum, label = TRUE)) %>%
  dplyr::select(Weekday, Steps, Sleep)

# Plot sleepday mean

plot_sleep_mean <- plot_ly(weekday_mean) %>%
  add_markers(x=~Weekday, y=~Sleep, text = ~Sleep, size = ~Sleep,
              marker = list(sizeref=0.1, sizemode="area", color = bb_colors[3], opacity = 0.6)) %>%
  layout(yaxis = list(title = "Sleep (Hours)"),
         xaxis = list(title = "", showgrid = FALSE),
         plot_bgcolor = bb_colors[6],
         paper_bgcolor = bb_colors[6])

# Plot step mean

plot_steps_mean <- plot_ly(weekday_mean) %>%
  add_markers(x=~Weekday, y=~Steps, text = ~Steps, size = ~Steps,
              marker=list(sizeref=0.1, sizemode="area", color = bb_colors[5], opacity = 0.6)) %>%
  layout(yaxis = list(title = "Steps"),
         xaxis = list(title = "", showgrid = FALSE),
         plot_bgcolor = bb_colors[6],
         paper_bgcolor = bb_colors[6])


# Import weight data and reformat

weight_log <- read.csv("weightLogInfo_merged.csv") %>% 
  dplyr::select(-Fat) %>%
  mutate(Time= hms::as_hms(mdy_hms(Date)),
         Date = as.Date(mdy_hms(Date))) %>% .$Id %>% unique(.) %>% length()

# Count weight by days

weight_count <- weight_log %>%
  group_by(Id) %>%
  count()

# Create table for days counts by each activity type

consistency_count <- daily_act_count %>%
  rename(Active = n) %>%
  left_join(sleep_day_count, by = "Id") %>%
  rename(Sleep = n) %>%
  left_join(weight_count, by = "Id") %>%
  rename(Weight = n) %>%
  left_join(daily_activity_wide %>% # Mutate dataframe for total steps & calculate mean by Id
              dplyr::select(Id, TotalSteps) %>% 
              group_by(Id) %>%
              summarize(Steps = n()), by = "Id") %>%
              #summarize(Steps = round(mean(TotalSteps),0)), by = "Id") %>%
  mutate(Sleep = ifelse(is.na(Sleep), 0, Sleep),
         Weight = ifelse(is.na(Weight), 0, Weight),
         W_S = ifelse(Weight > 0 & Sleep > 0, 1,0),
         W_Only = ifelse(Weight > 0 & Sleep == 0, 1, 0)) %>% # Change NA to 0 
  ungroup() 

# Calculate mean by activity type

consistency_mean <- consistency_count %>%
  summarise(Active = mean(Active),
            Sleep = mean(Sleep),
            Steps = mean(Steps),
            Weight = mean(Weight),
            )

# Sum day counts greater than zero (control for non-use)

consistency_sum <- consistency_count %>%
  summarise(Active = sum(Active > 0),
            Sleep = sum(Sleep > 0),
            Weight_Sleep = sum(W_S > 0),
            Weight_Only = sum(W_Only > 0)#,
            #Weight = sum(Weight > 0),
            #Steps = sum(Steps > 0)
  ) %>%
  mutate(ID = "ID") %>%
  pivot_longer(names_to = "Metric", values_to = "Value", -ID)

# Plot usage by user activity

plot_usuage_count <- plot_ly(consistency_sum) %>%
  add_bars(x=~Metric, y=~Value, color=~Metric, colors = bb_colors[c(2,3,4,1)], opacity = 0.6) %>%
  add_text(x=~Metric, y=~Value, text=~Value) %>%
  layout(xaxis = list(categoryorder = "total descending"),
         plot_bgcolor = bb_colors[6],
         paper_bgcolor = bb_colors[6]) %>%
  hide_legend()

# Create table of frequency of use

frequency_use <- sleep_minutes %>%
  mutate(Metric = "Sleep") %>%
  bind_rows(daily_activity[c(1:2, 4:5)] %>%
              dplyr::filter(Metric %in% c("SedentaryMinutes", "TotalActiveMinutes"))) %>%
  bind_rows(weight_log[1:3] %>%
              mutate(Metric = 'Weight') %>%
              rename(Value = WeightKg)) %>%
  pivot_wider(names_from = "Metric", values_from = "Value") %>%
  rename(Sedentary = SedentaryMinutes,
         Active = TotalActiveMinutes) %>%
  rowwise() %>%
  mutate(Total = sum(c_across(c(2:5)), na.rm = T),
         Use = ifelse(Total>0, 1, 0)) %>%
  ungroup() %>%
  group_by(Id) %>%
  summarise(Days = sum(Use))

# Histogram of total days

plot_freq_total <- plot_ly() %>%
  add_histogram(x=frequency_use$Days, type = "histogram", 
                marker = list(color = bb_colors[1], opacity = 0.6)) %>%
  layout(yaxis=list(title = "Users"),
         xaxis=list(title = "Total Days"),
         plot_bgcolor=bb_colors[6],
         paper_bgcolor=bb_colors[6])

# Histogram of active users

plot_freq_active <- plot_ly() %>%
  add_histogram(x=consistency_count$Active, type = "histogram", 
                marker = list(color = bb_colors[2], opacity = 0.6)) %>%
  layout(yaxis=list(title = "Users"),
         xaxis=list(title = "Active Days"),
         plot_bgcolor=bb_colors[6],
         paper_bgcolor=bb_colors[6])

# Histogram of sleep

plot_freq_sleep <- plot_ly() %>%
  add_histogram(x=consistency_count$Sleep, type = "histogram", 
                marker = list(color = bb_colors[3], opacity = 0.6)) %>%
  layout(yaxis=list(title = "Users"),
         xaxis=list(title = "Sleep Days"),
         plot_bgcolor=bb_colors[6],
         paper_bgcolor=bb_colors[6])

# Histogram of weight

plot_freq_weight <- plot_ly() %>%
  add_histogram(x=consistency_count$Weight, type = "histogram", 
                marker = list(color = bb_colors[4], opacity = 0.6)) %>%
  layout(yaxis=list(title = "Users"),
         xaxis=list(title = "Weight Days"),
         plot_bgcolor=bb_colors[6],
         paper_bgcolor=bb_colors[6])

# Import sleep data and shape to long format

sleep_day <- read.csv("sleepDay_merged.csv") %>%
  pivot_longer(names_to = "Metric", values_to = "Value", -c(Id, SleepDay)) %>% 
  mutate(SleepDay = mdy_hms(SleepDay),
         Activity = "Sleep") %>%
  rename(Date = SleepDay) %>%
  dplyr::select(Id, Date, Activity, Metric, Value)

# Summary statistics daily activity

daily_activity_sum <- daily_activity %>%
  dplyr::filter(Metric %in% c("TotalSteps", "TotalDistance", "SedentaryMinutes")) %>%
  group_by(Metric) %>%
  dplyr::select(Metric, Value) %>%
  split(.$Metric) %>% 
  map(summary)

# Summary statistics sleep activity

sleep_day_sum <- sleep_day %>%
  dplyr::filter(Metric %in% c("TotalSleepRecords","TotalMinutesAsleep", "TotalTimeInBed")) %>%
  group_by(Metric) %>%
  dplyr::select(Metric, Value) %>%
  split(.$Metric) %>% 
  map(summary)

# Import sleep data and summarize for minutes

sleep_minutes <- read_csv("minuteSleep_merged.csv") %>%
  mutate(Time= hms::as_hms(mdy_hms(date)),
         Date = as.Date(mdy_hms(date))) %>%
  dplyr::select(-date) %>%
  group_by(Id, Date) %>%
  mutate(Value = 1) %>%
  summarize(Value = sum(Value)) %>%
  mutate(Metric = "Sleep_Minutes") %>%
  select(Id, Date, Metric, Value)

# Segment activity data by level

activity_minutes <- daily_activity_wide %>%
  dplyr::select(Id, ActivityDate, VeryActiveMinutes, FairlyActiveMinutes, LightlyActiveMinutes, SedentaryMinutes) %>%
  'colnames<-'(c("Id", "Date", "Very", "Fairly", "Lightly", "Sedentary")) %>%
  rowwise() %>%
  mutate(Very = ifelse(is.na(Very),0,Very), # Remove NA if present
         Fairly = ifelse(is.na(Fairly),0,Fairly),
         Lightly = ifelse(is.na(Lightly),0,Lightly),
         Active_Minutes = sum(Very, Fairly, Lightly)) %>%
  ungroup() %>%
  pivot_longer(names_to = "Metric", values_to = "Value", -c(Id, Date))

# Calucate mean activity by level

activity_level <- activity_minutes %>%
  dplyr::filter(Metric != "Active_Minutes") %>%
  group_by(Metric) %>%
  summarise(Activity = mean(Value))

# Create table of activity by steps

steps_cals <- daily_activity_wide %>%
  dplyr::select(Id, ActivityDate, TotalSteps, TotalDistance, VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance, Calories) %>%
  'colnames<-'(c("Id", "Date", "Steps", "Distance", "Very", "Fairly", "Lightly", "Calories")) %>%
  mutate(StepMile = Steps/Distance,
         Very = ifelse(Very == 0, NA, Very * StepMile),
         Fairly = ifelse(Fairly == 0, NA, Fairly * StepMile),
         Lightly = ifelse(Lightly == 0, NA, Lightly * StepMile),
         Calories = ifelse(Calories == 0, NA, Calories))

# Check relationship between steps and calories
fit <- lm(Calories ~ Steps, data = steps_cals)

# SCATTER plot of calories and steps

plot_steps_cals <- plot_ly(steps_cals) %>%
  add_markers(x=~Very, y=~Calories, name = "Very",
              marker = list(color = bb_colors[1], opacity = 0.6)) %>%
  add_markers(x=~Fairly, y=~Calories, name = "Fairly",
              marker = list(color = bb_colors[2], opacity = 0.6)) %>%
  add_markers(x=~Lightly, y=~Calories, name = "Lightly",
              marker = list(color = bb_colors[3], opacity = 0.6)) %>%
  layout(yaxis = list(title = "Calories"),
         xaxis = list(title = "Steps", showgrid = FALSE),
         legend = list(x = 0, y = 100, orientation = "h"),
         plot_bgcolor = bb_colors[6],
         paper_bgcolor = bb_colors[6])

# Clean minutes data for complete days

minutes_total <- sleep_minutes %>%
  bind_rows(activity_minutes) %>%
  dplyr::filter(Metric %in% c("Active_Minutes", "Sleep_Minutes", "Sedentary")) %>%
  pivot_wider(names_from = "Metric", values_from = "Value") %>%
  rename(Sleep = Sleep_Minutes,
         Active = Active_Minutes) %>%
  mutate(Sleep = ifelse(is.na(Sleep),0,Sleep),
         Sedentary = ifelse(is.na(Sedentary),0,Sedentary),
         Active = ifelse(is.na(Active),0,Active),
         Total = Sleep + Sedentary + Active) %>%
  dplyr::filter(Total == 1440) 

# Summary statistics by activity type

minutes_sum <- minutes_total[3:5] %>%
  dplyr::filter(Sleep != 0,
                Sedentary != 0) %>% 
  summary() 

# BOX and WHISKERS plot of by activity in minutes

plot_minutes_sum <- plot_ly() %>%
  add_boxplot(y=~minutes_sum$Sleep, name = "Sleep",
              color = I(bb_colors[3]), opacity = 0.9) %>%
  add_boxplot(y=~minutes_sum$Active, name = "Active",
              color = I(bb_colors[2]), opacity = 0.9) %>%
  add_boxplot(y=~minutes_sum$Sedentary, name = "Sedentary",
              color = I(bb_colors[1]), opacity = 0.9) %>%
  hide_legend() %>%
  layout(yaxis = list(title = "Minutes"),
         plot_bgcolor = bb_colors[6],
         paper_bgcolor = bb_colors[6])





