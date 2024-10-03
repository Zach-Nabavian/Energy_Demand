library("tidyverse")
library("ggplot2")
library("scales")


energy <- read_csv("Desktop/Portfolio Projects/Energy Demand/energy_dataset.csv")

# Graph total energy demand over time
ggplot(data = energy, aes(x = time, y = `total load actual`)) +
  labs(title = "Total Demand") +
  geom_line()

# Focus on a single day in 2015
energy %>% 
  filter(year(time) == 2015, month(time) == 1, day(time) == 1) %>% 
  ggplot(aes(x = time, y = `total load actual`)) + 
  labs(title = "Total Demand") +
  geom_line()

# create individual columns for year, month, day, hour
energy <- energy %>% 
    mutate(year = year(time), month = month(time), day = day(time), hour = hour(time)) %>% 
    select_all()

# Use a graph to check for seasonality in hour of day
energy %>% 
  group_by(hour) %>%
  ggplot(aes(x = hour, y = `total load actual`)) +
  labs(title = "Hourly Demand") +
  scale_x_continuous(breaks = breaks_pretty()) +
  geom_boxplot(aes(group = hour))

# Use a graph the check for seasonality in month of year
energy %>% 
  group_by(month) %>% 
  ggplot(aes(x = month, y = `total load actual`)) +
  labs(title = "Monthly Demand") +
  scale_x_continuous(breaks = breaks_pretty()) +
  geom_boxplot(aes(group = month))

# Check for missing values of actual demand
energy %>% 
  filter(is.na(`total load forecast`)) %>% 
  select_all()

# Plot histogram of residuals of demand forecast and actual demand
energy %>% 
  filter(is.na(`total load actual`) == FALSE) %>% 
  mutate(resids = `total load forecast` - `total load actual`) %>% 
  ggplot(mapping = aes(resids)) +
  labs(title = "Total Load Forecast Residuals") +
  geom_histogram(bins = 100)

# Perform the Shapiro-Wilk to test to see if the residuals are normally distributed
resids = energy$`total load forecast` - energy$`total load actual`
sample = sample(resids, size = 5000)
shapiro.test(sample)

# Fill in missing values with lagged values from previous 24 hours 
lag <- lag(energy$`total load actual`, 24)

energy <- energy %>% 
    mutate(`total load actual` = if_else(is.na(`total load actual`), lag, `total load actual`)) %>% 
    select_all()

# Check for autocorrelation in total demand
demand_lag = energy$`total load actual`
acf(demand_lag)

# Use STL decomposition to check the seasonal and trend components in demand
demand <- ts(energy$`total load actual`, frequency = 24)
stl <- stl(demand, "periodic")
plot(stl)

# Format the stl object as tibble to later use ggplot 2 to graph the seasonal and trend components
dcmp <- as_tibble(stl$time.series)
dcmp

dcmp %>%
  ggplot(aes(x = energy$time, y = seasonal)) +
  labs(title = "Seasonal Component") +
  geom_line(position = position_jitter(w = 100, h = 100))
