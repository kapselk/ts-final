library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)
library(gridExtra)
library(zoo)
library(e1071)

df <- read_csv("USDEUR.csv", col_types = cols(.default = col_character())) %>%
  select(`End Date`, `USD/EUR`) %>%            
  rename(
    Date  = `End Date`,
    Price = `USD/EUR`
  ) %>%
  mutate(
    Date  = ymd(Date),                          
    Price = as.double(Price)                    
  ) %>%
  arrange(Date)

glimpse(df)
head(df)
summary(df$Price)

p1 <- ggplot(df, aes(x = Date, y = Price)) +
  geom_line() +
  labs(title = "USD/EUR Exchange Rate (2008-2013)",
       x = "Date", y = "Exchange Rate") +
  theme_minimal()

df <- df %>%
  mutate(Price_MA30 = rollmean(Price, k = 30, fill = NA, align = "right"))

p2 <- ggplot(df, aes(x = Date)) +
  geom_line(aes(y = Price), alpha = 0.4) +
  geom_line(aes(y = Price_MA30), size = 1, linetype = "solid") +
  labs(title = "USD/EUR & 30-Day Moving Average",
       x = "Date", y = "Rate") +
  theme_minimal()
grid.arrange(p1, p2, ncol = 1)

df_summary <- df %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(
    n        = n(),
    Mean     = mean(Price, na.rm = TRUE),
    SD       = sd(Price, na.rm = TRUE),
    Min      = min(Price, na.rm = TRUE),
    Max      = max(Price, na.rm = TRUE),
    Skewness = skewness(Price, na.rm = TRUE),
    Kurtosis = kurtosis(Price, na.rm = TRUE)
  )
print(df_summary)

p3 <- ggplot(df %>% mutate(Year = factor(year(Date))),
             aes(x = Year, y = Price)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Distribution of USD/EUR by Year",
       x = "Year", y = "Exchange Rate") +
  theme_minimal()
print(p3)

df <- df %>%
  mutate(
    LogPrice = log(Price),
    Return   = LogPrice - lag(LogPrice)
  )

df_return_summary <- df %>%
  filter(!is.na(Return)) %>%
  mutate(Year = year(Date)) %>%
  group_by(Year) %>%
  summarize(
    MeanRet = mean(Return, na.rm = TRUE),
    SDRet   = sd(Return, na.rm = TRUE),
    MinRet  = min(Return, na.rm = TRUE),
    MaxRet  = max(Return, na.rm = TRUE)
  )
print(df_return_summary)

p4 <- ggplot(df, aes(x = Date, y = Return)) +
  geom_line(color = "darkgreen") +
  labs(title = "Daily Log-Returns of USD/EUR",
       x = "Date", y = "Log-Return") +
  theme_minimal()

p5 <- ggplot(df %>% filter(!is.na(Return)), aes(x = Return)) +
  geom_histogram(aes(y = ..density..), bins = 50, fill = "gray80") +
  geom_density() +
  labs(title = "Histogram & Density of Log-Returns",
       x = "Log-Return", y = "Density") +
  theme_minimal()

print(p5)
grid.arrange(p4, p5, ncol = 1)

adf_price  <- adf.test(na.omit(df$Price), alternative = "stationary")
adf_return <- adf.test(na.omit(df$Return), alternative = "stationary")
print(adf_price)
print(adf_return)

ggtsdisplay(na.omit(df$Return),
            main = "ACF & PACF of USD/EUR Log-Returns")