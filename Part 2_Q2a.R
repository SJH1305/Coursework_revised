# Part 2, Q2a, What are the best times and days of the week to minimise delays each year?
# Reading in relevant files:
df_2004 <- read.csv("2004.csv.bz2")
df_2005 <- read.csv("2005.csv.bz2")
df_2006 <- read.csv("2006.csv.bz2")
df_2007 <- read.csv("2007.csv.bz2")
df_2008 <- read.csv("2008.csv.bz2")
airports <- read.csv("airports.csv")
carriers <- read.csv("carriers.csv")
planedata <- read.csv("plane-data.csv")
variabledescriptions <- read.csv("variable-descriptions.csv")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(skimr)

# Understanding the data:
str(df_2004)
variabledescriptions

# Delays in each year
## Creating new variable - TotalDelay:
## Carrierdelay + Weatherdelay + Nasdelay + Securitydelay + Lateaircraftdelay

## using mutate while considering NaN values (set = 0) 
dfs <- list('2004' = df_2004, '2005' = df_2005, '2006' = df_2006,
             '2007' = df_2004, '2008' = df_2008)

for (year in names(dfs)){
  dfs[[year]] <- dfs[[year]] %>% 
    mutate(Delay = ifelse(is.na(CarrierDelay), 0, CarrierDelay) + 
            ifelse(is.na(WeatherDelay), 0 , WeatherDelay) +
            ifelse(is.na(NASDelay), 0 , NASDelay) + 
            ifelse(is.na(SecurityDelay), 0 , SecurityDelay) + 
            ifelse(is.na(LateAircraftDelay), 0 , LateAircraftDelay))
}

## Average delay for each year

avgdelay_years <- c(
  "2004" = mean(df_2004$Delay, na.rm = FALSE),
  "2005" = mean(df_2005$Delay, na.rm = FALSE),
  "2006" = mean(df_2006$Delay, na.rm = FALSE),
  "2007" = mean(df_2007$Delay, na.rm = FALSE),
  "2008" = mean(df_2008$Delay, na.rm = FALSE)
)
print (avgdelay_years)

barplot1 <- barplot (avgdelay_years, main = "Average delay per year", ylab = "Average delay")
png("Barplot - Yearly avg_delay.png", width = 8, height = 5, units = 'in', res = 300)

# Best times to minimise delay
## Count of number of uniquely scheduled flight times
unique_times <- numeric(length(dfs))
names(unique_times) <- names(dfs)
for (year in names(dfs)){
  unique_times[year] <-length(unique(dfs[[year]]$CRSDepTime)) 
}
print(unique_times)
#length(unique(df_2004$CRSDepTime)) 

## Visualisation - 4 time segments 
# 2004
df_2004 <- df_2004 %>% filter(!is.na(CRSDepTime))

df_2004 <- df_2004 %>% 
  mutate(Time_segment = 
    ifelse(CRSDepTime>=0 & CRSDepTime<600, "12AM-6AM",
    ifelse(CRSDepTime>=600 & CRSDepTime<1200, "6AM-12PM",
    ifelse(CRSDepTime>=1200 & CRSDepTime<1800, "12PM-6PM",
    ifelse(CRSDepTime>=1800 & CRSDepTime<2359, "6PM-12AM", NA)))))

ggplot(df_2004, aes(x = Time_segment, y = Delay)) +
  geom_boxplot(fill = "lightblue")+
  geom_jitter(alpha = 0.2, color = "orange") +
  labs(title = "Delays throughout the day - 2004",
       x = "Segments of Day",
       y = "Delay (min)")
ggsave("Boxplot(times)_2004.png", width = 8, height = 5, units = 'in')

#2005
df_2005 <- df_2005 %>% filter(!is.na(CRSDepTime))

df_2005 <- df_2005 %>% 
  mutate(Time_segment = 
          ifelse(CRSDepTime>=0 & CRSDepTime<600, "12AM-6AM",
          ifelse(CRSDepTime>=600 & CRSDepTime<1200, "6AM-12PM",
          ifelse(CRSDepTime>=1200 & CRSDepTime<1800, "12PM-6PM",
          ifelse(CRSDepTime>=1800 & CRSDepTime<2359, "6PM-12AM", NA)))))

ggplot(df_2005, aes(x = Time_segment, y = Delay)) +
  geom_boxplot(fill = "lightblue")+
  geom_jitter(alpha = 0.2, color = "orange") +
  labs(title = "Delays throughout the day - 2005",
       x = "Segments of Day",
       y = "Delay (min)")
ggsave("Boxplot(times)_2005.png", width = 8, height = 5, units = 'in')

#2006
df_2006 <- df_2006 %>% filter(!is.na(CRSDepTime))

df_2006 <- df_2006 %>% 
  mutate(Time_segment = 
          ifelse(CRSDepTime>=0 & CRSDepTime<600, "12AM-6AM",
          ifelse(CRSDepTime>=600 & CRSDepTime<1200, "6AM-12PM",
          ifelse(CRSDepTime>=1200 & CRSDepTime<1800, "12PM-6PM",
          ifelse(CRSDepTime>=1800 & CRSDepTime<2359, "6PM-12AM", NA)))))

ggplot(df_2006, aes(x = Time_segment, y = Delay)) +
  geom_boxplot(fill = "lightblue")+
  geom_jitter(alpha = 0.2, color = "orange") +
  labs(title = "Delays throughout the day - 2006",
       x = "Segments of Day",
       y = "Delay (min)")
ggsave("Boxplot(times)_2006.png", width = 8, height = 5, units = 'in')

#2007
df_2007 <- df_2007 %>% filter(!is.na(CRSDepTime))

df_2007 <- df_2007 %>% 
  mutate(Time_segment = 
          ifelse(CRSDepTime>=0 & CRSDepTime<600, "12AM-6AM",
          ifelse(CRSDepTime>=600 & CRSDepTime<1200, "6AM-12PM",
          ifelse(CRSDepTime>=1200 & CRSDepTime<1800, "12PM-6PM",
          ifelse(CRSDepTime>=1800 & CRSDepTime<2359, "6PM-12AM", NA)))))

ggplot(df_2007, aes(x = Time_segment, y = Delay)) +
  geom_boxplot(fill = "lightblue")+
  geom_jitter(alpha = 0.2, color = "orange") +
  labs(title = "Delays throughout the day - 2007",
       x = "Segments of Day",
       y = "Delay (min)")
ggsave("Boxplot(times)_2007.png", width = 8, height = 5, units = 'in')

#2008
df_2008 <- df_2008 %>% filter(!is.na(CRSDepTime))

df_2008 <- df_2008 %>% 
  mutate(Time_segment = 
          ifelse(CRSDepTime>=0 & CRSDepTime<600, "12AM-6AM",
          ifelse(CRSDepTime>=600 & CRSDepTime<1200, "6AM-12PM",
          ifelse(CRSDepTime>=1200 & CRSDepTime<1800, "12PM-6PM",
          ifelse(CRSDepTime>=1800 & CRSDepTime<2359, "6PM-12AM", NA)))))

ggplot(df_2008, aes(x = Time_segment, y = Delay)) +
  geom_boxplot(fill = "lightblue")+
  geom_jitter(alpha = 0.2, color = "orange") +
  labs(title = "Delays throughout the day - 2008",
       x = "Segments of Day",
       y = "Delay (min)")
ggsave("Boxplot(times)_2008.png", width = 8, height = 5, units = 'in')

## Average delay - in terms of scheduled departures 
avg_timedelay <- list()
for (year in names(dfs)){
  avg_df <- dfs[[year]] %>% 
  group_by(CRSDepTime) %>% 
  summarise(AvgDelay = mean(Delay, na.rm = TRUE)) %>% 
  mutate(Year = as.integer(year))
  avg_timedelay[[year]] <- avg_df
}

#avg_timedelay_2004 <- df_2004 %>% 
  #group_by(CRSDepTime) %>% 
  #summarise(avg_timedelay_04 = mean(Delay, na.rm = TRUE))
#avg_timedelay_2004 <- avg_timedelay_2004 %>% 
  #rename(AvgDelay = avg_timedelay_04) %>% mutate(Year = 2004)

avg_timedelay_all <- bind_rows(avg_timedelay) 
print(avg_timedelay_all)
# -----------------------------------------------------
# Best days to minimise delay 

## Average delay in terms of days of the week 
avgdailydelay <- list()
for (year in names(dfs)) {
  avg_daily <- dfs[[year]] %>% 
  group_by(DayOfWeek) %>% 
  summarise(Avg_Dailydelay = mean(Delay, na.rm = TRUE)) %>% 
  mutate(Year = as.integer(year))
}
avgdailydelay <- avg_daily
avgdailydelay_all <- bind_rows(avgdailydelay_all)
print(avgdailydelay_all)

## barplot 
ggplot(avgdailydelay_all) + 
  geom_bar(aes(x = factor(DayOfWeek), y = Avg_Dailydelay), stat = "identity") +
labs(x = "Days (1 = Mon, 7 = Sun)",
     y = "Average Delay (min)")
ggsave("Bar plot - avg_delay,days.png", width = 8, height = 5, units = 'in')
