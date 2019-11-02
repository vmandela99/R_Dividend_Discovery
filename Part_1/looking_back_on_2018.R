# http://www.reproduciblefinance.com/2019/01/14/looking-back-on-last-year/
library(dplyr)
library(riingo)
library(tibble)
library(lubridate)
library(ggplot2)

ticker = c("XLY", "XLP", "XLE",
          "XLF", "XLV", "XLI", "XLB",
          "XLK", "XLU", "XLRE",
          "SPY")
sector = c("Consumer Discretionary", "Consumer Staples", "Energy",
          "Financials", "Health Care", "Industrials", "Materials",
          "Information Technology", "Utilities", "Real Estate",
          "Market")

etf_ticker_sector <- tibble(
  ticker = ticker,
  sector = sector
)

# using 'pull()' to get a specific column
tickers <- etf_ticker_sector %>%
  pull(ticker)

# create an API key
# riingo_browse_signup()
# riingo_browse_token()
token <- "57182996116879c63622566bb75b35d86a1aab20"
riingo_set_token("57182996116879c63622566bb75b35d86a1aab20")

prices <- etf_ticker_sector %>%
  pull(ticker) %>%
  riingo_prices(.,
                start_date = "2017-12-29",
                end_date = "2018-12-31"
                )
glimpse(prices)

# change date
prices <- prices %>%
  mutate(date = ymd(date))
glimpse(prices)

# join 'etf_ticker_sector' above with 'prices'
prices <- prices %>%
  left_join(etf_ticker_sector, by = "ticker")
glimpse(prices)

# check sector labels by grouping and taking the first row of each group
prices_check <- prices %>%
  group_by(ticker) %>%
  slice(1)
glimpse(prices_check)

# calculate daily returns of each sector
sector_returns_2018 <- prices %>%
  select(sector, date, adjClose) %>%
  group_by(sector) %>%
  mutate(daily_return = log(adjClose) - log(lag(adjClose)))
glimpse(sector_returns_2018)

sector_returns_2018 <- sector_returns_2018 %>%
  na.omit()
glimpse(sector_returns_2018)

# plot the returns
sector_returns_2018 %>%
  ggplot(aes(x = date, y = daily_return, color = sector)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sector) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  scale_y_continuous(labels = function(x)paste0(x, "%")) +
  labs(x = "", y = "daily returns")

# create a positeve column
sector_returns_2018_pos_neg <- sector_returns_2018 %>%
  mutate(col_pos = if_else(daily_return > 0, daily_return, as.numeric(NA)))
sector_returns_2018_pos_neg <- sector_returns_2018_pos_neg %>%
  mutate(col_neg = if_else(daily_return < 0, daily_return, as.numeric(NA)))
glimpse(sector_returns_2018_pos_neg)

# plot the positive/negetive columns
sector_returns_2018_pos_neg %>%
  ggplot(aes(x = date)) +
  geom_col(aes(y = col_neg), alpha = .85, fill = "pink", color = "pink") +
  geom_col(aes(y = col_pos), alpha = .85, fill = "cornflowerblue", color = "cornflowerblue") +
  facet_wrap(~sector) +
  labs(title = "2018 daily returns", y = "daily returns" ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1),
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    strip.background = element_blank(),
    strip.placement = "inside",
    strip.text = element_text(size = 15),
    panel.spacing = unit(0.2, "lines"),
    panel.background = element_rect(fill = "gray"))


