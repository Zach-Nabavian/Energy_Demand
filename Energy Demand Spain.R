#update.packages(ask = FALSE)
library("tidyverse")
library("ggplot2")
library("scales")
library("fable")
library("tsibble")
library("feasts")
library("gt")
library('urca')
library("tseries")
library("lubridate")
library("forecast")
library("data.table")


energy <- read_csv("energy_dataset.csv")
weather <- read_csv("weather_features.csv")

# Get an average temperature for each timestamp
weather_grouped <- weather %>% 
  group_by(dt_iso) %>% 
  summarize(mean_temp = mean(temp))
  
# Join together the energy dataset and the mean temperature dataset
energy <- inner_join(energy, weather_grouped, by = c("time" = "dt_iso"))

# Add a moving average column for temperature
energy <- energy %>% 
  mutate(moving_avg = frollmean(mean_temp, 24))

# Graph total energy demand over time
energy_all_time <- ggplot(data = energy, aes(x = time, y = `total load actual`)) +
    labs(title = "Energy Demand Over Whole Dataset") +
    geom_line() +
    theme_bw() +
    theme(axis.title = element_text(size = 15, face = "bold")) +
    theme(text = element_text(size = 14)) +
    theme(title = element_text(size = 15, face = "bold"))

# Focus on a single day in 2015
energy_in_2015 <- energy %>% 
    filter(year(time) == 2015 & month(time) == 1 & day(time) == 1) %>% 
    ggplot(aes(x = time, y = `total load actual`)) + 
    labs(title = "Actual Demand in January 2015") +
    geom_line() +
    theme_bw() +
    theme(axis.title = element_text(size = 15, face = "bold")) +
    theme(text = element_text(size = 14)) +
    theme(title = element_text(size = 15, face = "bold"))

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
  mutate(weekend = if_else(weekday %in% c("Saturday", "Sunday"), "Yes", "No")) %>% 
  select_all()

energy %>% 
  select(time, weekday, weekend) %>% 
  print(n  = 1000)

# Use a graph to check for seasonality in hour of day
hour_box_plot <- energy %>% 
    group_by(hour) %>%
    ggplot(aes(x = hour, y = `total load actual`)) +
    geom_boxplot(aes(group = hour)) +
    labs(title = "Spanish Energy Demand Shows a Large Hourly Effect", y = "Total Load Actual (GWh)", x = "Hour") +
    scale_x_continuous(breaks = seq(0, 23, 1)) +
    theme_bw() +
    theme(axis.title = element_text(size = 15, face = "bold")) +
    theme(text = element_text(size = 14)) +
    theme(title = element_text(size = 15, face = "bold"))


# Use a graph the check for seasonality in month of year
month_box_plot <- energy %>% 
    group_by(month) %>% 
    ggplot(aes(x = month, y = `total load actual`)) +
    geom_boxplot(aes(group = month)) +
    labs(title = "Spanish Energy Demand Shows a Small Monthly Seasonal Effect", y = "Total Load Actual (GWh)", x = "Month") +
    scale_x_continuous(breaks = seq(1, 12, 1)) +
    theme_bw() +
    theme(axis.title = element_text(size = 15, face = "bold")) +
    theme(text = element_text(size = 14)) +
    theme(title = element_text(size = 15, face = "bold"))

energy$weekday <- factor(energy$weekday, levels = c(
  "Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Use a graph the check for seasonality for the day of the week
week_box_plot <- energy %>% 
    group_by(weekday) %>% 
    ggplot(aes(x = weekday, y = `total load actual`)) +
    geom_boxplot(aes(group = weekday)) + 
    labs(title = "Spanish Energy Demand Shows Lower Demand on Weekends", y = "Total Load Actual (GWh)", x = "Weekday") +
    theme_bw() +
    theme(axis.title = element_text(size = 15, face = "bold")) +
    theme(text = element_text(size = 14)) +
    theme(title = element_text(size = 15, face = "bold"))

# Perform the Shapiro-Wilk to test to see if the residuals are normally distributed
resids = energy$`total load forecast` - energy$`total load actual`
sample = sample(resids, size = 5000)
shapiro.test(sample)

# Use the Kruskal-Wallis test to check for seasonality by hour, month, and weekday
kruskal.test(`total load actual` ~ hour, data = energy)
kruskal.test(`total load actual` ~ month, data = energy)
kruskal.test(`total load actual` ~ weekday, data = energy)

# Filter for just Jan 2015
energy_jan_2015 <- energy %>% 
    filter(year == 2015 & month == 1) %>% 
    select(time, hour, weekday, weekend, `total load actual`)

energy_jan_2015 <- as_tsibble(energy_jan_2015)

# Filter for first half of February
energy_feb_2015 <- energy %>% 
      filter(year == 2015 & month == 2 & day <= 14) %>% 
      select(time, `total load actual`)

energy_feb_2015 <- as_tsibble(energy_feb_2015)

feb_2015_plot <- energy_feb_2015 %>% 
    autoplot(`total load actual`)

jan_2015_features <- energy_jan_2015 %>% 
    gg_tsdisplay(difference(`total load actual`, lag = 24, differences = 1), plot_type = 'partial')

# Model some simple forecasting methods and regression model
demand_fit <- energy_jan_2015 %>% 
    model(
      mean = MEAN(`total load actual`),
      snaive_hour = SNAIVE(`total load actual` ~ lag(24)),
      snaive_weekday = SNAIVE(`total load actual` ~ lag (7)),
      etsa = ETS(`total load actual` ~ error("A") + trend("N") + season("A")),
      etsm = ETS(`total load actual` ~ error("M") + trend("N") + season("M")),
      arima200 = ARIMA(`total load actual` ~ pdq(2, 0, 0) + PDQ(2, 1, 0)),
      #arima_weekly = ARIMA(`total load actual` ~ pdq(2, 0, 0) + PDQ(2, 1, 0) + (fourier(period = "week", K = 5)))
    )

# forecast for the next day
simple_fc <- demand_fit %>% 
    forecast(h = 336)

short_term_forecast <- simple_fc %>%
    filter(.model == 'etsm' | .model == 'etsa') %>% 
    autoplot(energy_jan_2015, level = NULL) +
    autolayer(energy_feb_2015, color = 'grey') +
    labs(y = "Total Load Actual (GWh)", title = 'Forecasting Hourly Demand') +
    guides(color = guide_legend(title = 'Forecast')) +
    theme_bw() +
    theme(axis.title = element_text(size = 15, face = "bold")) +
    theme(text = element_text(size = 14)) +
    theme(title = element_text(size = 15, face = "bold")) +
    scale_x_datetime(date_breaks = "3 days")

# Generate diagnostic plots for the mean forecast residuals
short_term_mean_residuals <- energy_feb_2015 %>% 
    model(mean = MEAN(`total load actual`)) %>% 
    gg_tsresiduals()

# Generate diagnostic plots for the naive seasonal forecast residuals, treating the hour of the day as the season
naive_hour_residuals <- energy_feb_2015 %>% 
    model(snaive_hour = SNAIVE(`total load actual` ~ lag(24))) %>% 
    gg_tsresiduals()

# Generate diagnostic plots for the naive seasonal forecast residuals, treating the day of the week as the season
naive_week_residuals <- energy_feb_2015 %>% 
    model(snaive_weekday = SNAIVE(`total load actual` ~ lag(7))) %>% 
    gg_tsresiduals()
  
etsa_residuals <- energy_feb_2015 %>% 
    model(etsa = ETS(`total load actual` ~ error("A") + trend("N") + season("A"))) %>% 
    gg_tsresiduals()

etsm_residuals <- energy_feb_2015 %>% 
    model(etsm = ETS(`total load actual` ~ error("M") + trend("N") + season("M"))) %>% 
    gg_tsresiduals()

arima_residuals <- energy_feb_2015 %>% 
  model(arima = ARIMA(`total load actual` ~ pdq(2, 0, 0) + PDQ(1, 1, 0))) %>% 
  gg_tsresiduals() +
  labs(title = 'Arima200 Residuals Summary')

demand_fit %>% 
  augment() %>% 
  features(.innov, box_pierce, lag = 24)

# Create a table showing point estimate accuracy measures for the simple forecasts
point_estimates <- accuracy(simple_fc, energy_feb_2015) %>% 
  arrange(RMSE) %>% 
  select(.model, ME, RMSE, MAE, MPE, MAPE)

short_term_table <- gt(point_estimates) %>% 
    tab_header(title = "Short Term Forecast Accuracy Measures") %>% 
    fmt_number(decimals = 2) %>% 
    fmt_percent(columns = c("MPE", "MAPE"), scale_values = FALSE) %>% 
    tab_style(style = list(
      cell_text(weight = "bold")
      ), locations = cells_title()) 

# Develop forecast for longer time frame

# Filter for first three years of dataset

energy_first_three_years <- energy %>% 
  filter(year <= 2017) %>% 
  mutate_at(c("hour", "month"), as.factor) %>%
  select(time, hour, weekday, weekend, month, `total load actual`, mean_temp, moving_avg) 

energy_first_three_years <- as_tsibble(energy_first_three_years)

energy_last_year <- energy %>% 
  filter(year == 2018) %>% 
  mutate(temp_poly = poly(mean_temp, 3),
         moving_avg_poly = poly(moving_avg, 3),
         hour = factor(hour, levels = levels(energy_first_three_years$hour)),
         weekday = factor(weekday, levels = levels(energy_first_three_years$weekday)),
         month = factor(month, levels = levels(energy_first_three_years$month))) %>% 
  select(time, hour, weekday, weekend, month, `total load actual`, mean_temp, temp_poly, moving_avg, moving_avg_poly)

energy_last_year <- as_tsibble(energy_last_year)

# Model some simple forecasting methods and regression model
demand_fit_long_term <- energy_first_three_years %>%
  filter(is.na(moving_avg) == FALSE) %>%
  model(
    mean = MEAN(`total load actual`),
    snaive_hour = SNAIVE(`total load actual` ~ lag(24)),
    snaive_weekday = SNAIVE(`total load actual` ~ lag (7)),
    etsa = ETS(`total load actual` ~ error("A") + trend("N") + season("A")),
    etsm = ETS(`total load actual` ~ error("M") + trend("N") + season("M")),
    arima200 = ARIMA(`total load actual` ~ pdq(2, 0, 0) + PDQ(2, 1, 0)),
    #arima_weekly = ARIMA(`total load actual` ~ pdq(2, 0, 0) + PDQ(2, 1, 0) + (fourier(period = "week", K = 5))),
    regression = TSLM(`total load actual` ~ trend() + month + weekday + hour + weekday:hour + poly(mean_temp, 3) +
                        poly(mean_temp,3):month + poly(mean_temp,3):hour),
    regression_recency = TSLM(`total load actual` ~ trend() + month + weekday + hour + weekday:hour + poly(mean_temp, 3) +
                                poly(mean_temp,3):month + poly(mean_temp,3):hour + poly(moving_avg, 3))
   ) 

# forecast the next year
simple_fc_long_term <- demand_fit_long_term %>% 
  forecast(new_data = energy_last_year)

demand_fit_long_term %>% 
  augment() %>% 
  features(.innov, box_pierce, lag = 36)

point_estimates <- accuracy(simple_fc_long_term, energy_last_year) %>% 
  arrange(RMSE) %>% 
  select(.model, ME, RMSE, MAE, MPE, MAPE)

long_term_measures_table <- gt(point_estimates) %>% 
    tab_header(title = "Long Term Forecast Accuracy Measures") %>% 
    fmt_number(decimals = 2) %>% 
    fmt_percent(columns = c("MPE", "MAPE"), scale_values = FALSE) %>% 
    tab_style(style = list(
      cell_text(weight = "bold")
    ), locations = cells_title())

# Check residual diagnostics for the harmonic regression models
energy_last_year %>% 
  model(
    regression = TSLM(`total load actual` ~ trend() + month + weekday + hour + weekday:hour + poly(mean_temp, 3) +
                      poly(mean_temp,3):month + poly(mean_temp,3):hour)
    ) %>% 
  gg_tsresiduals() +
  labs(title = "Regression Model Residuals Summary")

temp_energy_scatter <- energy %>% 
    ggplot(aes(x = mean_temp, y = `total load actual`)) +
    geom_point() +
    labs(y = "Total Load Actual (GWh)", x = "Temerature (Kelvin)", title = "Nonlinear Relationship Between Temperature and Demand") +
    theme_bw() +
    theme(axis.title = element_text(size = 15, face = "bold")) +
    theme(text = element_text(size = 14)) +
    theme(title = element_text(size = 15, face = "bold"))
