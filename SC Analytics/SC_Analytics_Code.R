# Load libraries
library(dplyr)
library(ggplot2)
library(forecast)
library(lubridate)

# Import data
data <- read.csv("Global_Superstore2.csv")

# Data selection
inventory <- data %>%
  # filtering the data to get only furnishing products
  filter(grepl("Furnishings", Sub.Category, ignore.case = TRUE)) %>%
  # selecting only relevant features to the research objectives
  select(Order.Date, Ship.Date, Product.Name, Sales, Quantity, Discount, Profit)

# Supply Chain Analysis -> Trends
# Data Description
summary(inventory[c("Sales", "Quantity", "Discount")])

# Visualization of Quantity Distribution
ggplot(inventory, aes(x = Quantity)) +
  geom_histogram(fill = "#0c4c8a", color = "#0c4c8a", bins = 30) +
  labs(title = "Distribution of Quantity for Furnishing Products", x = "Sales", y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 15, 1))

# Get the inventory by day
inventory_by_day <- inventory %>%
  mutate(date = as.POSIXct(Order.Date, format = "%d-%m-%Y")) %>%
  mutate(Day = as.Date(date)) %>%
  group_by(Day) %>%
  summarise(TotalQuantity = sum(Quantity))

# Plot line graph to analyze trends
ggplot(data = inventory_by_day, aes(x = Day, y = TotalQuantity)) +
  geom_line() +
  labs(x = "Day", y = "Total Quantity", title = "Daily Quantity Trend")

# Get the inventory by month
inventory_by_month <- inventory %>%
  mutate(date = as.POSIXct(Order.Date, format = "%d-%m-%Y")) %>%
  mutate(Month = month(date)) %>%
  group_by(Month) %>%
  summarise(TotalQuantity = sum(Quantity))

# Plot line graph to analyze trends
ggplot(inventory_by_month, aes(x = Month, y = TotalQuantity)) +
  geom_line() +
  labs(x = "Month", y = "Total Quantity", title = "Monthly Quantity Trend") +
  scale_x_continuous(breaks = seq(0, 13, 1))

# Get the inventory by year
inventory_by_year <- inventory %>%
  mutate(date = as.POSIXct(Order.Date, format = "%d-%m-%Y")) %>%
  mutate(Year = year(date)) %>%
  group_by(Year) %>%
  summarise(TotalQuantity = sum(Quantity))

# Plot line graph to analyze trends
ggplot(inventory_by_year, aes(x = Year, y = TotalQuantity)) +
  geom_line() +
  labs(x = "Year", y = "Total Quantity", title = "Yearly Quantity Trend") +
  theme_minimal()

# Get the inventory by year_month
inventory_by_year_month <- inventory %>%
  mutate(date = as.POSIXct(Order.Date, format = "%d-%m-%Y")) %>%
  mutate(Year_Month = floor_date(date, unit = "month")) %>%
  group_by(Year_Month) %>%
  summarise(TotalQuantity = sum(Quantity))

# Plot line graph to analyze trends
ggplot(inventory_by_year_month, aes(x = Year_Month, y = TotalQuantity)) +
  geom_line() +
  labs(x = "Year_Month", y = "Total Quantity", title = "Yearly Quantity Trend") +
  theme_minimal()

mean(inventory_by_day$TotalQuantity)
sd(inventory_by_day$TotalQuantity)

# forecasting analysis
inventory_ts <- ts(inventory_by_year_month$TotalQuantity, start = c(2011, 1), frequency = 12)
inventory_forecast <- forecast(auto.arima(inventory_ts), h = 12)
last_month <- tail(inventory_by_year_month$Year_Month, 1)
forecast_dates <- seq(as.Date(last_month, format = "%Y-%m-%d"), by = "month", length.out = 12)
inventory_forecast_df <- data.frame(date = forecast_dates, 
                                    forecast = inventory_forecast$mean,
                                    lower = inventory_forecast$lower[,2], 
                                    upper = inventory_forecast$upper[,2])

# Plot the Forecast
ggplot(inventory_forecast_df, aes(x = date)) +
  geom_line(aes(y = forecast), color = "blue") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "blue") +
  labs(x = "Year-Month", y = "Total Quantity", title = "Forecast of Total Quantity for 2015") +
  ggtitle("Inventory Forecast for 2015") +
  theme_bw()
