# https://www.r-bloggers.com/dividend-sleuthing-with-r/

library(dplyr)
library(riingo)
library(tibble)
library(lubridate)
library(ggplot2)
library(ggrepel)
library(tidyquant)

riingo::riingo_set_token("57182996116879c63622566bb75b35d86a1aab20")
# get the companies in sp500
?tidyquant::tq_index
(options <- tidyquant::tq_index_options())
sp_500 <- tq_index("SP500", use_fallback = TRUE)
glimpse(sp_500)

# pull symbols from supported tickers
riingo_tickers <- supported_tickers() %>%
  select(ticker) %>%
  pull()
glimpse(riingo_tickers)

# select top 30 companies ordered by 'weight' column
tickers <- sp_500 %>%
  arrange(desc(weight)) %>%
  slice(1:31) %>%
  filter(symbol %in% riingo_tickers) %>%
  select(symbol,company)
glimpse(tickers)

# get the prices
?riingo::riingo_prices
# Note: this takes a minute or two to complete
prices <- riingo_prices(tickers$symbol, start_date = "1990-01-01", end_date = "2018-12-31") %>%
  mutate(date = ymd(date)) %>%
  select(date, ticker, close, divCash) %>%
  arrange(ticker)
glimpse(prices)

# group by symbol, get only dividends > 0 and get the last row of each group
dividend_2018 <- prices %>%
  group_by(ticker) %>%
  filter(date > "2017-12-31" & divCash > 0) %>%
  slice(n())
glimpse(dividend_2018)

# plot dividend's absolute cash payout
dividend_2018 %>%
  ggplot(aes(x = date, y = divCash, color = ticker)) +
  geom_point() +
  geom_text_repel(aes(label = ticker)) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = NULL,
    y = "div/share",
    title = "2018 Dividends cash payout: Top SP 500 companies"
  ) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# compute the yeild: total dividend for entire year and divide by closing price at the first dividend date
yield_2018 <- prices %>%
  filter(date > "2017-12-31" & divCash > 0) %>%
  group_by(ticker) %>%
  mutate(div_total = sum(divCash)) %>%
  arrange(ticker)
glimpse(yield_2018)

# get the first row from each group -- which gives us the closing price at the first dividend date
yield_2018 <- yield_2018 %>%
  slice(1)
glimpse(yield_2018)

# compute yield column
yield_2018 <- yield_2018 %>%
  mutate(div_yield = div_total/close)
glimpse(yield_2018)

# plot yield for 2018
ggplot(yield_2018, aes(x = date, y = div_yield, color = ticker)) +
  geom_point() +
  geom_text_repel(aes(label = ticker)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
  labs(
    x = NULL,
    y = "yield",
    title = "2018 Dividends yield: Top SP 500 companies"
  ) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))

# compute dividend across all years
dividend_all_years <- prices %>%
  mutate(year = year(date)) %>%
  group_by(ticker,year) %>%
  mutate(div_total = sum(divCash)) %>%
  slice(n()) %>%
  select(year, ticker, div_total)
glimpse(dividend_all_years)

?dplyr::case_when
?dplyr::lag


# create 'div_total_diff' and 'div_increase' variables
dividend_all_years <- dividend_all_years %>%
  group_by(ticker) %>%
  mutate(
    div_total_diff = div_total - lag(div_total),
    div_increase = case_when((div_total - lag(div_total) > 0) ~ 1, TRUE ~ 0)
  ) %>%
  #arrange(desc(year)) %>%
  arrange(ticker)
glimpse(dividend_all_years)


# look at MSFT
dividend_MSFT <- dividend_all_years %>%
  filter(ticker == "MSFT")
glimpse(dividend_MSFT)

# count the number of consecutive dividend increases
# get every 'div_increase' equal to 0
dividend_MSFT_0 <- dividend_MSFT %>%
  slice(which(div_increase == 0))
glimpse(dividend_MSFT_0)

#get the first instance of div_increase = 0
dividend_MSFT_0_first <- dividend_MSFT %>%
  slice(min(which(div_increase == 0)))
glimpse(dividend_MSFT_0_first)

dividend_seq <- dividend_all_years %>%
  arrange(desc(year)) %>%
  arrange(ticker) %>%
  slice(seq_len(min(which(div_increase == 0))))
glimpse(dividend_seq)

# order the years of increase so company with longest increase is at the top
dividend_seq_order <- dividend_seq %>%
  mutate(div_inc_consec = sum(div_increase)) %>%
  slice(1) %>%
  arrange(desc(div_inc_consec))
glimpse(dividend_seq_order)

# plot the results
ggplot(dividend_seq_order, aes(x = reorder(ticker, div_inc_consec), y = div_inc_consec, fill = ticker)) +
  geom_col(width = .5) +
  geom_label_repel(aes(label = ticker), color = "white", nudge_y = .6) +
  theme(
    legend.position = "none",
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = "",
    y = "years consec div increase"
  ) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))

