library(tidyverse)
library(lubridate)
library(forecast)
library(rugarch)

train_year <- 2010
test_year  <- 2011

df <- read_csv("USDEUR.csv",
               col_types = cols(.default = col_character())) %>%
  select(`End Date`, `USD/EUR`) %>%
  rename(Date = `End Date`, Price = `USD/EUR`) %>%
  mutate(
    Date  = ymd(Date),
    Price = as.double(Price)
  ) %>%
  arrange(Date) %>%
  mutate(
    LogPrice = log(Price),
    Return   = LogPrice - lag(LogPrice)
  )

df <- df %>% filter(!is.na(Return))

train_end   <- max(df$Date[year(df$Date) == train_year])
test_dates  <- df %>% filter(year(Date) == test_year) %>% pull(Date)
n_test      <- length(test_dates)

fc <- tibble(
  Date   = test_dates,
  Actual = df %>% filter(Date %in% test_dates) %>% pull(Price),
  RW     = NA_real_,
  SES    = NA_real_,
  HOLT   = NA_real_,
  ARIMA  = NA_real_,
  GARCH = NA_real_    # if you implement GARCH
)

for(i in seq_along(test_dates)) {
  today   <- test_dates[i]
  hist    <- df %>% filter(Date < today)
  rets    <- hist$Return
  last_pr <- hist$Price %>% last()
  
  # Random-walk (naive): forecast = 0 return
  rw_ret   <- 0
  
  # Simple Exponential Smoothing
  ses_ret  <- ses(rets, h = 1)$mean
  
  # Holt's linear method
  holt_ret <- holt(rets, h = 1)$mean
  
  # ARIMA via auto.arima()
  arima_ret <- forecast(auto.arima(rets), h = 1)$mean
  
  #  GARCH(1,1) on returns
  spec    <- ugarchspec(
              mean.model     = list(armaOrder=c(0,0)),
               variance.model = list(garchOrder=c(1,1)),
               distribution.model = "norm"
            )
  fit_g   <- ugarchfit(spec, rets, solver = "hybrid")
  garch_ret <- as.numeric(ugarchforecast(fit_g, n.ahead = 1)@forecast$seriesFor)
  
  ## back-transform to prices
  #    \hat p_t = p_{t-1} * exp(\hat r_t)
  fc$RW[i]    <- last_pr * exp(rw_ret)
  fc$SES[i]   <- last_pr * exp(ses_ret)
  fc$HOLT[i]  <- last_pr * exp(holt_ret)
  fc$ARIMA[i] <- last_pr * exp(arima_ret)
  fc$GARCH[i] <- last_pr * exp(garch_ret)
}

sse <- fc %>%
  summarize(across(RW:GARCH,
                   ~ sum((Actual - .x)^2),
                   .names = "SSE_{col}"))
print(sse)

best_sse_col <- names(sse)[ which.min(unlist(sse)) ]  
best_model   <- sub("^SSE_", "", best_sse_col)  
message("Best model: ", best_model)

library(ggplot2)
p <- fc %>%
  select(Date, Actual, all_of(best_model)) %>%
  pivot_longer(-Date, names_to = "Series", values_to = "Price") %>%
  ggplot(aes(x = Date, y = Price, color = Series)) +
  geom_line() +
  labs(title = sprintf("Actual vs %s Forecast (%d->%d)", 
                       best_model, train_year, test_year),
       x = "Date", y = "Exchange Rate") +
  theme_minimal()

print(p)
ggsave(sprintf("best_forecast_%s_%d_%d.png",
               tolower(best_model), train_year, test_year),
       plot = p, width = 8, height = 4)

