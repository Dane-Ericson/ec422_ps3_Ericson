dir.create("data")
library(tidyverse)

head(retail_raw)

retail_raw_long <- retail_raw |>
   select(observation_date, RSAFS, RSAFSNA) |>
   pivot_longer(cols = c(RSAFS, RSAFSNA), names_to = "series_id", values_to = "sales")
head(retail_raw_long)

library(fpp3)

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
