# Q2b - Evaluate whether older planes suffer more delays on a year-to-year basis.
library(tidyverse)
library(dplyr)
library(ggplot2)
library(skimr)

# Understanding the data:
str(planedata)
str(df_2004)

# planes in plane data appear more than once in each df
df_2004 %>% 
  count(TailNum) %>% 
  arrange(desc(n)) %>% 
  filter(n > 1)

# Identifying and cross referencing flights in 'planedata' with each df

### This is because planes data contains all known planes, we only want
### relevant planes for each year '''

#Exclude 0 and 00000 flight numbers
removed <- list()
for (year in names(dfs)) {
  removed[[year]] <- dfs[[year]] %>% 
    filter(!TailNum %in% c("0","00000")) 
}
#remove_2004 <- df_2004 %>% 
  #filter(!TailNum %in% c("0","00000")) 

### I only need TailNum, tailnum, year, age, Delay and
### only need to add the year column
actuallyflew_2004 <- df_2004 %>% filter(TailNum %in% planedata$tailnum) %>% 
  left_join(planedata %>%  select(tailnum, year), by = c("TailNum" = "tailnum"))
actuallyflew_2005 <- df_2005 %>% filter(TailNum %in% planedata$tailnum) %>% 
  left_join(planedata %>%  select(tailnum, year), by = c("TailNum" = "tailnum"))
actuallyflew_2006 <- df_2006 %>% filter(TailNum %in% planedata$tailnum) %>% 
  left_join(planedata %>%  select(tailnum, year), by = c("TailNum" = "tailnum"))
actuallyflew_2007 <- df_2007 %>% filter(TailNum %in% planedata$tailnum) %>% 
  left_join(planedata %>%  select(tailnum, year), by = c("TailNum" = "tailnum"))
actuallyflew_2008 <- df_2008 %>% filter(TailNum %in% planedata$tailnum) %>% 
  left_join(planedata %>%  select(tailnum, year), by = c("TailNum" = "tailnum"))

## Creating age variable using function (calc.)
actuallyflew_2004 <- actuallyflew_2004 %>% 
  filter(!is.na(year) & year != 0) %>% 
  mutate(year = as.numeric(year), age = 2004 - year) 
actuallyflew_2005 <- actuallyflew_2005 %>% 
  filter(!is.na(year) & year != 0) %>% 
  mutate(year = as.numeric(year), age = 2004 - year) 
actuallyflew_2006 <- actuallyflew_2006 %>% 
  filter(!is.na(year) & year != 0) %>% 
  mutate(year = as.numeric(year), age = 2004 - year) 
actuallyflew_2007 <- actuallyflew_2007 %>% 
  filter(!is.na(year) & year != 0) %>% 
  mutate(year = as.numeric(year), age = 2004 - year) 
actuallyflew_2008 <- actuallyflew_2008 %>% 
  filter(!is.na(year) & year != 0) %>% 
  mutate(year = as.numeric(year), age = 2004 - year) 

# 'Delay' is already added into the environment in Q2a 
a_dfs <- list('2004' = actuallyflew_2004, '2005' = actuallyflew_2005, '2006' = actuallyflew_2006,
            '2007' = actuallyflew_2007, '2008' = actuallyflew_2008)

for (year in names(a_dfs)){
  a_dfs[[year]] <- a_dfs[[year]] %>% 
    mutate(Delay = ifelse(is.na(CarrierDelay), 0, CarrierDelay) + 
             ifelse(is.na(WeatherDelay), 0 , WeatherDelay) +
             ifelse(is.na(NASDelay), 0 , NASDelay) + 
             ifelse(is.na(SecurityDelay), 0 , SecurityDelay) + 
             ifelse(is.na(LateAircraftDelay), 0 , LateAircraftDelay))
}
actuallyflew_2004 <- a_dfs[['2004']]
actuallyflew_2005 <- a_dfs[['2005']]
actuallyflew_2006 <- a_dfs[['2006']]
actuallyflew_2007 <- a_dfs[['2007']]
actuallyflew_2008 <- a_dfs[['2008']]

# Distribution of delayed flights - Avg delay of individual planes & age
for (year in names(actuallyflew)) {
  actuallyflew[[year]] <- actuallylflew[[year]] %>% 
    group_by(TailNum) %>% 
    mutate(idv_age= mean(Delay, na.rm = TRUE)) %>% 
    ungroup()
}

ggplot(actuallyflew_2004,aes(x = idv_avg)) +
  geom_histogram(aes(y = ..density..)) +
labs(title = "Distribution of individual avg.delays and Age(2004)",
     x = "Average delay per plane(min)",
     y = "F.Density")
ggsave("Histogram - Individual avg.delay (2004).png", width = 8, height = 5, units = 'in')

ggplot(actuallyflew_2005,aes(x = idv_avg)) +
  geom_histogram(aes(y = ..density..)) +
labs(title = "Distribution of individual avg.delays and Age(2005)",
     x = "Average delay per plane(min)",
     y = "F.Density")
ggsave("Histogram - Individual avg.delay (2005).png", width = 8, height = 5, units = 'in')

ggplot(actuallyflew_2006,aes(x = idv_avg)) +
  geom_histogram(aes(y = ..density..)) +
labs(title = "Distribution of individual avg.delays and Age(2006)",
     x = "Average delay per plane(min)",
     y = "F.Density")
ggsave("Histogram - Individual avg.delay (2006).png", width = 8, height = 5, units = 'in')

ggplot(actuallyflew_2007,aes(x = idv_avg)) +
  geom_histogram(aes(y = ..density..)) +
labs(title = "Distribution of individual avg.delays and Age(2007)",
     x = "Average delay per plane(min)",
     y = "F.Density")
ggsave("Histogram - Individual avg.delay (2007).png", width = 8, height = 5, units = 'in')

ggplot(actuallyflew_2008,aes(x = idv_avg)) +
  geom_histogram(aes(y = ..density..)) +
labs(title = "Distribution of individual avg.delays and Age(2008)",
     x = "Average delay per plane(min)",
     y = "F.Density")
ggsave("Histogram - Individual avg.delay (2008).png", width = 8, height = 5, units = 'in')

# Distribution of delayed flights - Avg delay & plane age 
avgdelay_2004 <- actuallyflew_2004 %>% 
  filter(age >= 0, age <= 50) %>% 
  group_by(age) %>% 
  summarise(AvgDelay = mean(Delay, na.rm = TRUE)) %>% 
  ungroup()

avgdelay_2005 <- actuallyflew_2005 %>% 
  filter(age >= 0, age <= 50) %>% 
  group_by(age) %>% 
  summarise(AvgDelay = mean(Delay, na.rm = TRUE)) %>% 
  ungroup()

avgdelay_2006 <- actuallyflew_2006 %>% 
  filter(age >= 0, age <= 50) %>% 
  group_by(age) %>% 
  summarise(AvgDelay = mean(Delay, na.rm = TRUE)) %>% 
  ungroup()

avgdelay_2007 <- actuallyflew_2007 %>% 
  filter(age >= 0, age <= 50) %>% 
  group_by(age) %>% 
  summarise(AvgDelay = mean(Delay, na.rm = TRUE)) %>% 
  ungroup()

avgdelay_2008 <- actuallyflew_2008 %>% 
  filter(age >= 0, age <= 50) %>% 
  group_by(age) %>% 
  summarise(AvgDelay = mean(Delay, na.rm = TRUE)) %>% 
  ungroup()

ggplot(avgdelay_2004, aes(x = age, y = AvgDelay)) +
  geom_line(color = "steelblue") +
  labs(title = "Line plot of Plane age and Average Delays(2004)",
       x = "Plane age (years)",
       y = "Average Delay (min)")
ggsave("Lineplot - Plane age and Average Delays(2004).png", width = 8, height = 5, units = 'in')

ggplot(avgdelay_2005, aes(x = age, y = AvgDelay)) +
  geom_line(color = "steelblue") +
  labs(title = "Line plot of Plane age and Average Delays(2005)",
       x = "Plane age (years)",
       y = "Average Delay (min)")
ggsave("Lineplot - Plane age and Average Delays(2005).png", width = 8, height = 5, units = 'in')

ggplot(avgdelay_2006, aes(x = age, y = AvgDelay)) +
  geom_line(color = "steelblue") +
  labs(title = "Line plot of Plane age and Average Delays(2006)",
       x = "Plane age (years)",
       y = "Average Delay (min)")
ggsave("Lineplot - Plane age and Average Delays(2006).png", width = 8, height = 5, units = 'in')

ggplot(avgdelay_2007, aes(x = age, y = AvgDelay)) +
  geom_line(color = "steelblue") +
  labs(title = "Line plot of Plane age and Average Delays(2007)",
       x = "Plane age (years)",
       y = "Average Delay (min)")
ggsave("Lineplot - Plane age and Average Delays(2007).png", width = 8, height = 5, units = 'in')

ggplot(avgdelay_2008, aes(x = age, y = AvgDelay)) +
  geom_line(color = "steelblue") +
  labs(title = "Line plot of Plane age and Average Delays(2008)",
       x = "Plane age (years)",
       y = "Average Delay (min)")
ggsave("Lineplot - Plane age and Average Delays(2008).png", width = 8, height = 5, units = 'in')

# Distribution of non-delay flights - Histogram 
ontime_2004 <- actuallyflew_2004 %>% 
  filter(Delay == 0)

ontime_2005 <- actuallyflew_2005 %>% 
  filter(Delay == 0)

ontime_2006 <- actuallyflew_2006 %>% 
  filter(Delay == 0)

ontime_2007 <- actuallyflew_2007 %>% 
  filter(Delay == 0)

ontime_2008 <- actuallyflew_2008 %>% 
  filter(Delay == 0)

ggplot(ontime_2004 %>% filter(age > 0, age < 50), aes(x = age)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(color = "steelblue") +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2004)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - Non delayed flights,2004.png", width = 10, height = 5, units = 'in')

ggplot(ontime_200 5%>% filter(age > 0, age < 50), aes(x = age)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(color = "steelblue") +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2005)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - Non delayed flights,2005.png", width = 10, height = 5, units = 'in')

ggplot(ontime_2006 %>% filter(age > 0, age < 50), aes(x = age)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(color = "steelblue") +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2006)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - Non delayed flights,2006.png", width = 10, height = 5, units = 'in')
 
ggplot(ontime_2007 %>% filter(age > 0, age < 50), aes(x = age)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(color = "steelblue") +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2007)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - Non delayed flights,2007.png", width = 10, height = 5, units = 'in')

ggplot(ontime_2008 %>% filter(age > 0, age < 50), aes(x = age)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(color = "steelblue") +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2008)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - Non delayed flights,2008.png", width = 10, height = 5, units = 'in')

# Distribution of Aircraft age
ggplot(actuallyflew_2004 %>% filter(age > 0, age < 50), aes(x = age))+
  geom_histogram(aes(y = ..density..)) +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2004)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - age distribution per year (2004).png", width = 10, height = 5, units = 'in')

ggplot(actuallyflew_2005 %>% filter(age > 0, age < 50), aes(x = age))+
  geom_histogram(aes(y = ..density..)) +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2005)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - age distribution per year (2005).png", width = 10, height = 5, units = 'in')

ggplot(actuallyflew_2006 %>% filter(age > 0, age < 50), aes(x = age))+
  geom_histogram(aes(y = ..density..)) +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2006)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - age distribution per year (2006).png", width = 10, height = 5, units = 'in')

ggplot(actuallyflew_2007 %>% filter(age > 0, age < 50), aes(x = age))+
  geom_histogram(aes(y = ..density..)) +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2007)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - age distribution per year (2007).png", width = 10, height = 5, units = 'in')

ggplot(actuallyflew_2008 %>% filter(age > 0, age < 50), aes(x = age))+
  geom_histogram(aes(y = ..density..)) +
  labs(title = "Frequency density of On-time flights with respect of Aircraft Age (2008)",
       x = "Age(years)",
       y = "F.Density")
ggsave("Histogram - age distribution per year (2008).png", width = 10, height = 5, units = 'in')

# Why 'issue date' was not chosen
# list storage
actuallyflew <- list('2004' = actuallyflew_2004,
                     '2005' = actuallyflew_2005,
                     '2006' = actuallyflew_2006,
                     '2007' = actuallyflew_2007,
                     '2008' = actuallyflew_2008)

negativeage_year_count <- numeric(length(actuallyflew))
negativeage_id_count <- numeric(length(actuallyflew))
names(negativeage_year_count) <- names(actuallyflew)
names(negativeage_id_count) <- names(actuallyflew)

for (year in names(actuallyflew)) {
  df <- actuallyflew[[year]] %>% 
    left_join(planedata %>% select(tailnum, issue_date), by = c("TailNum" = "tailnum"))
  df$issue_date <- as.Date(df$issue_date)
  df$age_id <- as.integer(year) - as.numeric(format(df$issue_date, "%Y"))

  negativeage_year_count[year] <- sum(df$age < 0, na.rm = TRUE)
  negativeage_id_count[year] <- sum(df$age_id < 0, na.rm = TRUE)
}
print("NegativeCounts based on 'issue date':")
print(negativeage_year_count)
print("NegativeCounts based on 'issue date':")
print(negativeage_id_count)


negativeage_count04 <- sum(actuallyflew_2004$age <0, na.rm = TRUE)
negativeage_count05 <- sum(actuallyflew_2005$age <0, na.rm = TRUE)
negativeage_count06 <- sum(actuallyflew_2006$age <0, na.rm = TRUE)
negativeage_count07 <- sum(actuallyflew_2007$age <0, na.rm = TRUE)
negativeage_count08 <- sum(actuallyflew_2008$age <0, na.rm = TRUE)

# add issue date column 
actuallyflew_2004 <- actuallyflew_2004 %>% 
  left_join(planedata %>% select(tailnum, issue_date), by = c("TailNum" = "tailnum"))
actuallyflew_2005 <- actuallyflew_2005 %>% 
  left_join(planedata %>% select(tailnum, issue_date), by = c("TailNum" = "tailnum"))
actuallyflew_2006 <- actuallyflew_2006 %>% 
  left_join(planedata %>% select(tailnum, issue_date), by = c("TailNum" = "tailnum"))
actuallyflew_2007 <- actuallyflew_2007 %>% 
  left_join(planedata %>% select(tailnum, issue_date), by = c("TailNum" = "tailnum"))
actuallyflew_2008 <- actuallyflew_2008 %>% 
  left_join(planedata %>% select(tailnum, issue_date), by = c("TailNum" = "tailnum"))

actuallyflew_2004$issue_date <- as.Date(actuallyflew_2004$issue_date)
actuallyflew_2005$issue_date <- as.Date(actuallyflew_2005$issue_date)
actuallyflew_2006$issue_date <- as.Date(actuallyflew_2006$issue_date)
actuallyflew_2007$issue_date <- as.Date(actuallyflew_2007$issue_date)
actuallyflew_2008$issue_date <- as.Date(actuallyflew_2008$issue_date)

actuallyflew_2004$age_id <- 2004 - as.numeric(format(actuallyflew_2004$issue_date, "%Y"))
actuallyflew_2005$age_id <- 2005 - as.numeric(format(actuallyflew_2005$issue_date, "%Y"))
actuallyflew_2006$age_id <- 2006 - as.numeric(format(actuallyflew_2006$issue_date, "%Y"))
actuallyflew_2007$age_id <- 2007 - as.numeric(format(actuallyflew_2007$issue_date, "%Y"))
actuallyflew_2008$age_id <- 2008 - as.numeric(format(actuallyflew_2008$issue_date, "%Y"))

negativeage_count_id04 <- sum(actuallyflew_2004$age_id < 0, na.rm = TRUE)
negativeage_count_id05 <- sum(actuallyflew_2005$age_id < 0, na.rm = TRUE)
negativeage_count_id06 <- sum(actuallyflew_2006$age_id < 0, na.rm = TRUE)
negativeage_count_id07 <- sum(actuallyflew_2007$age_id < 0, na.rm = TRUE)
negativeage_count_id08 <- sum(actuallyflew_2008$age_id < 0, na.rm = TRUE)

print("NegativeCounts based on 'issue date':",
      negativeage_count_id04, ",",
      negativeage_count_id05, ",",
      negativeage_count_id06, ",",
      negativeage_count_id07, ",",
      negativeage_count_id08, "\n")

print("NegativeCounts based on 'year'",
      negativeage_count04, ",",
      negativeage_count05, ",",
      negativeage_count06, ",",
      negativeage_count07, ",",
      negativeage_count08, "\n")