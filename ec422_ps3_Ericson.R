dir.create("data")
library(tidyverse)

retail_raw <- read_csv("data/retail_sales_raw.csv")
head(retail_raw)

retail_raw_long <- retail_raw |>
   select(observation_date, RSAFS, RSAFSNA) |>
   pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")
head(retail_raw_long)

library(fpp3)
retail_ts <- retail_raw_long |>
  mutate(Month = yearmonth(observation_date)) |>
  as_tsibble(index = Month, key = series_id)
head(retail_ts)

has_gaps(retail_ts)

head(canadian_gas)

key_vars(canadian_gas)

index_var(canadian_gas)

can_gas <- canadian_gas |>
      select(Month, Volume)

has_gaps(can_gas)

head(can_gas)

can_gas_recent <- can_gas |>
       filter(Month >= yearmonth("1990 Jan"))
 
head(can_gas_recent)

can_gas_recent <- can_gas_recent |>
       mutate(year = year(Month),
                          month   = month(Month, label = TRUE),
                          quarter = quarter(Month))
can_gas_recent |>
       as_tibble() |>
       group_by(month) |>
       summarize(avg_volume = mean(Volume))

can_gas_recent |>
  mutate(next_month           = Month + 1,
         same_month_last_year = Month - 12) |>
  head()

can_gas_q <- can_gas |>
       filter(Month <= yearmonth("2004 Dec")) |>
       index_by(Quarter = yearquarter(Month)) |>
       summarize(Volume = sum(Volume))

head(can_gas_q)

can_gas_q |> autoplot(Volume)
## We sum the monthly values for gas production rather than taking the mean, as we are trying to observe the total quantity produced over the period, rather than the average monthly production. In the autoplot, the quarterly series does appear to be seasonal. 
retail_ts |> autoplot(sales)
retail_ts |>
      autoplot(sales) +
      labs(
             title = "U.S. Retail and Food Services Sales",
             subtitle = "Seasonally adjusted vs. not seasonally adjusted",
             x = NULL, y = "Millions of dollars", color = NULL
         ) +
      theme_minimal()
##(2.7b): In the NSA data, we can observe seasonality in the form of spikes near the end of each year(December), and drops at the beginnigs of the year (January, February). We do not see this pattern in the seasonally adjusted data. The two series do appear to follow the same trend in the long run. 

##(2.7c)
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Gas Production",
    x = NULL, y = "Volume, In Billions of Cubic Meters" 
  ) +
  theme_minimal()

##(2.7d)
can_gas |>
  autoplot(Volume) +
  labs(
    title = "Canadian Gas Production",
    x = NULL, y = "Volume, In Billions of Cubic Meters" 
  ) +
  theme_minimal() +
  geom_smooth ()
## I decided to add a trend line using geom_smooth, in order to more easily display how the series changes over time through the seasonality.

##(Part 3)
##(3.1 Log transformation)
##(3.1a) The seasonal swings in our Canadian gas plot do appear to grow as the level of the data increases, particularly towards the middle of the plot. However, as the level of the data raises further towards the end of the plot, the size of the seasonal swings do not appear to grow, and may even decrease.
##(3.1b) 
can_gas <- can_gas |>
  mutate(log_volume = log(Volume))

can_gas |>
  autoplot(log_volume) +
  labs(
    title = "Canadian Gas Production (Log Transformed)",
    x = NULL, y = "Log of Volume, In Billions of Cubic Meters" 
  ) +
  theme_minimal() +
  geom_smooth ()

## The shape of the logged graph is different than the regular volume graph, as the level of the graph rises much more in the earlier years and is flatter in the later years on the graph. The seasonal swings do appear smaller, however the swings in the middle are still much more pronounced than the swings in the early and later years. I do not think logs are particularly useful for this graph, as while the seasonal variations do appear smaller, the variations in the middle are still disproportinately large compared to the variations at the ends, and the series is not notably more linear than the original.

##(3.1c) 
retail_nsa <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  mutate(
    log_sales = log(sales)
  )

retail_nsa |>
  autoplot(sales) +
  labs(
    title = "Non-Seasonally Adjusted Retail Sales",
    x = NULL,
    y = "Sales (Millions of Dollars)"
  ) +
  theme_minimal()

retail_nsa |>
  autoplot(log_sales) +
  labs(
    title = "Non-Seasonally Adjusted Retail Sales (Log Transformed)",
    x = NULL,
    y = "Log of Sales (Millions of Dollars)"
  ) +
  theme_minimal()

##The case for log transformation is stronger in the retail series, as it displays strong seasonality and upwards trend, with the log version of the graph appearing much more linear. 

can_gas <- can_gas |>
  mutate(
    growth_direct = 100 * difference(Volume)/lag(Volume),
    growth_log = 100 * difference(log_volume)
  )

can_gas |>
  autoplot(growth_direct) +
  labs(
    title = "Canadian Gas Production Growth (Direct)",
    x = NULL, y = "Growth Rate (%)" 
  ) +
  theme_minimal()

can_gas <- can_gas |>
  mutate(
    growth_gap = abs(growth_direct - growth_log)
  )

can_gas |>
  autoplot(growth_gap) +
  labs(
    title = "Canadian Gas Production Growth (Gap Between Direct and Log)",
    x = NULL, y = "Growth Rate Gap (%)" 
  ) +
  theme_minimal()

## I think the approximation is rather good, as the gap between the growth rates is small at most points, despite containing a few spikes in their difference.
##(3.3)
retail_ts <- retail_ts |>
  group_by_key() |>
  mutate(growth_mom = 100 * difference(log(sales))) |>
  ungroup()

retail_ts |>
  autoplot(growth_mom) +
  labs(
    title = "Month-over-Month Growth: SA vs. NSA Retail Sales",
    x = NULL, y = "Growth rate (%)", color = NULL
  ) +
  theme_minimal()

## The month-over-month growth rates for the NSA series appear to be much noisier, as it contains the seasonal fluctuations from seasonal spending patterns. The SA series removes this variation, making the underlying monthly changes more apparent, similarly to the differences between the SA and NSA series in problem 2.7b.

##(Part 4)

##(4.1)

retail_ts |>
  filter(series_id == "RSAFSNA") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in NSA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

retail_ts |>
  filter(series_id == "RSAFS") |>
  gg_season(sales) +
  labs(
    title = "Seasonal Pattern in SA Retail Sales",
    y = "Millions of dollars"
  ) +
  theme_minimal()

can_gas |>
  gg_season(log_volume) +
  labs(
    title = "Seasonal Pattern in Canadian Gas Production",
    y = "Log of Volume"
  ) +
  theme_minimal()

## The seasonal pattern in the can_gas data appears to describe higer production at the beginning and end of the year, with a dip from lower production in the middle months.

##(4.2)
can_gas |>
  gg_subseries(log_volume) +
  labs(
    title = "Subseries Plot: Canadian Gas Production",
    y = "log(Volume)"
  )

##(4.3)

install.packages("seasonal")


retail_nsa_x13 <- retail_ts |>
  filter(series_id == "RSAFSNA") |>
  model(x13 = X_13ARIMA_SEATS(sales)) |>
  components()

retail_nsa_x13 |> autoplot()

## The seasonal factor appear to be the strongest in December, typically being about 10%-20% above trend. The size of the seasonal swings appears to decrease over time.

sa_compare <- retail_nsa_x13 |>
  as_tibble() |>
  select(Month, mine = season_adjust) |>
  left_join(
    retail_ts |>
      filter(series_id == "RSAFS") |>
      as_tibble() |>
      transmute(Month, census = sales),
    by = "Month"
  ) |>
  mutate(pct_diff = 100 * (log(mine) - log(census)))

sa_compare |>
  ggplot(aes(x = Month, y = pct_diff)) +
  geom_line() +
  labs(
    title = "My X-13 Seasonal Adjustment Minus the Census Bureau's",
    x = NULL, y = "Difference (%)"
  ) +
  theme_minimal()

## The difference between my seasonal adjustment and the Census Bureau's appears to be very small, with the difference being less than 1% for most of the series. However, larger differences (over 1%) appear around 2020, likely due to the COVID-19 pandemic's impact on retail sales.

##(4.4)