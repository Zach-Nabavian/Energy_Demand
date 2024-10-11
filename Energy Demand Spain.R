library("tidyverse")
library("ggplot2")
library("scales")
library("fable")
library("tsibble")
library("feasts")

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
    mutate(year = year(time), month = month(time), day = day(time), hour = hour(time), weekday = weekdays.Date(time)) %>% 
    select_all()

# Fill in missing values with lagged values from previous 24 hours 
lag <- lag(energy$`total load actual`, 24)

# Check for missing values of actual demand
energy %>% 
  filter(is.na(`total load actual`)) %>% 
  select_all()

energy <- energy %>% 
  mutate(`total load actual` = if_else(is.na(`total load actual`), lag, `total load actual`)) %>% 
  select_all()

energy %>% 
  select(time, weekday)

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

energy$weekday <- factor(energy$weekday, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Use a graph the check for seasonality for the day of the week
energy %>% 
  group_by(weekday) %>% 
  ggplot(aes(x = weekday, y = `total load actual`)) +
  labs(title = "Daily Demand") +
  geom_boxplot(aes(group = weekday))

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

# Check for autocorrelation in total demand
demand_lag = energy$`total load actual`
acf(demand_lag)

# Use the Kruskal-Wallis test to check for seasonality by hour, month, and weekday
kruskal.test(`total load actual` ~ hour, data = energy)
kruskal.test(`total load actual` ~ month, data = energy)
kruskal.test(`total load actual` ~ weekday, data = energy)

# Filter for just Jan 2015
energy_jan_2015 <- energy %>% 
    filter(year == 2015 & month == 1) %>% 
    select(time, hour, weekday, `total load actual`)

energy_jan_2015 <- as_tsibble(energy_jan_2015)

# Filter for first half of February
energy_feb_2015 <- energy %>% 
      filter(year == 2015 & month == 2 & day <= 14) %>% 
      select(time, `total load actual`)

energy_feb_2015 <- as_tsibble(energy_feb_2015)

energy_jan_2015 %>% 
  autoplot(`total load actual`)

# Model some simple forecasting methods
demand_fit <- energy_jan_2015 %>% 
    model(
      #mean = MEAN(`total load actual`),
      #naive = NAIVE(`total load actual`),
      snaive_hour = SNAIVE(`total load actual` ~ lag(24)),
      snaive_weekday = SNAIVE(`total load actual` ~ lag (7)),
      #drift = RW(`total load actual` ~ drift())
    )

# forecast fort the next day
simple_fc <- demand_fit %>% 
    forecast(h = 336)

simple_fc %>% 
  autoplot(energy_jan_2015, level = NULL) +
  autolayer(energy_feb_2015, color = 'grey') +
  labs(y = 'demand', title = 'Simple Forecasts for Total Demand') +
  guides(color = guide_legend(title = 'Forecast'))

# Use the augment function to find the residuals for each of the forecasts
resid <- augment(demand_fit)

# Generate diagnostic plots for the mean forecast residuals
energy_feb_2015 %>% 
  model(mean = MEAN(`total load actual`)) %>% 
  gg_tsresiduals()

# Generate diagnostic plots for the naive seasonal forecast residuals, treating the hour of the day as the season
energy_feb_2015 %>% 
  model(snaive_hour = SNAIVE(`total load actual` ~ lag(24))) %>% 
  gg_tsresiduals()

# Generate diagnostic plots for the naive seasonal forecast residuals, treating the day of the week as the season
energy_feb_2015 %>% 
  model(snaive_weekday = SNAIVE(`total load actual` ~ lag(7))) %>% 
  gg_tsresiduals()
