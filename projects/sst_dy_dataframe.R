# Script file to read in Sea Surface Temperature data 

#-------------------------------------------------------------------------------
# Read in sea surface temperature dataset 1:
# Read raw text lines:
raw_line1 = readLines("projects/_data/sst0n95w_dy.ascii")
raw_line2 = readLines("projects/_data/sst2n95w_dy.ascii")
raw_line3 = readLines("projects/_data/sst5n95w_dy.ascii")
raw_line4 = readLines("projects/_data/sst8n95w_dy.ascii")
raw_line5 = readLines("projects/_data/sst2s95w_dy.ascii")
raw_line6 = readLines("projects/_data/sst5s95w_dy.ascii")
raw_line7 = readLines("projects/_data/sst8s95w_dy.ascii")

# Extract only lines starting with date:
data_lines1 = raw_line1[grepl("^[0-9]{8}\\s",str_trim(raw_line1))]
data_lines2 = raw_line2[grepl("^[0-9]{8}\\s",str_trim(raw_line2))]
data_lines3 = raw_line3[grepl("^[0-9]{8}\\s",str_trim(raw_line3))]
data_lines4 = raw_line4[grepl("^[0-9]{8}\\s",str_trim(raw_line4))]
data_lines5 = raw_line5[grepl("^[0-9]{8}\\s",str_trim(raw_line5))]
data_lines6 = raw_line6[grepl("^[0-9]{8}\\s",str_trim(raw_line6))]
data_lines7 = raw_line7[grepl("^[0-9]{8}\\s",str_trim(raw_line7))]

# Read cleaned lines into dataframe:
dat1 = read.table(text=data_lines1, header = FALSE)
dat2 = read.table(text=data_lines2, header = FALSE)
dat3 = read.table(text=data_lines3, header = FALSE)
dat4 = read.table(text=data_lines4, header = FALSE)
dat5 = read.table(text=data_lines5, header = FALSE)
dat6 = read.table(text=data_lines6, header = FALSE)
dat7 = read.table(text=data_lines7, header = FALSE)
colnames(dat1) = c("YYYYMMDD", "HHMM", "SST", "Q", "S")
colnames(dat2) = c("YYYYMMDD", "HHMM", "SST", "Q", "S")
colnames(dat3) = c("YYYYMMDD", "HHMM", "SST", "Q", "S")
colnames(dat4) = c("YYYYMMDD", "HHMM", "SST", "Q", "S")
colnames(dat5) = c("YYYYMMDD", "HHMM", "SST", "Q", "S")
colnames(dat6) = c("YYYYMMDD", "HHMM", "SST", "Q", "S")
colnames(dat7) = c("YYYYMMDD", "HHMM", "SST", "Q", "S")

# Clean up missing values:
dat1 = dat1 |>
  filter(SST!=-9.99) |>
  mutate(Date=as.Date(as.character(YYYYMMDD),format="%Y%m%d"))
dat2 = dat2 |>
  filter(SST!=-9.99) |>
  mutate(Date=as.Date(as.character(YYYYMMDD),format="%Y%m%d"))
dat3 = dat3 |>
  filter(SST!=-9.99) |>
  mutate(Date=as.Date(as.character(YYYYMMDD),format="%Y%m%d"))
dat4 = dat4 |>
  filter(SST!=-9.99) |>
  mutate(Date=as.Date(as.character(YYYYMMDD),format="%Y%m%d"))
dat5 = dat5 |>
  filter(SST!=-9.99) |>
  mutate(Date=as.Date(as.character(YYYYMMDD),format="%Y%m%d"))
dat6 = dat6 |>
  filter(SST!=-9.99) |>
  mutate(Date=as.Date(as.character(YYYYMMDD),format="%Y%m%d"))
dat7 = dat7 |>
  filter(SST!=-9.99) |>
  mutate(Date=as.Date(as.character(YYYYMMDD),format="%Y%m%d"))

# Sort data:
ts1 = dat1 |>
  mutate(Date=as.Date(Date)) |>
  as_tsibble(index=Date) |>
  fill_gaps()
ts2 = dat2 |>
  mutate(Date=as.Date(Date)) |>
  as_tsibble(index=Date) |>
  fill_gaps()
ts3 = dat3 |>
  mutate(Date=as.Date(Date)) |>
  as_tsibble(index=Date) |>
  fill_gaps()
ts4 = dat4 |>
  mutate(Date=as.Date(Date)) |>
  as_tsibble(index=Date) |>
  fill_gaps()
ts5 = dat5 |>
  mutate(Date=as.Date(Date)) |>
  as_tsibble(index=Date) |>
  fill_gaps()
ts6 = dat6 |>
  mutate(Date=as.Date(Date)) |>
  as_tsibble(index=Date) |>
  fill_gaps()
ts7 = dat7 |>
  mutate(Date=as.Date(Date)) |>
  as_tsibble(index=Date) |>
  fill_gaps()

# # Plot: 
# ggplot(data = ts1,
#        mapping = aes(x = Date,
#                      y = SST)) +  
#   geom_line(col = 'deeppink')  +
#   theme_minimal(base_size=14) +
#   labs(title = 'Sea surface temperature in the Pacific from 2000 to 2026.', 
#        x = 'Time (day)',
#        y = 'Sea surface temperature (C)')+
#   theme(
#     plot.title = element_text(hjust = 0.5)
#   )
# ggplot(data = ts2,
#        mapping = aes(x = Date,
#                      y = SST)) +  
#   geom_line(col = 'deeppink')  +
#   theme_minimal(base_size=14) +
#   labs(title = 'Sea surface temperature in the Pacific from 2000 to 2026.', 
#        x = 'Time (day)',
#        y = 'Sea surface temperature (C)')+
#   theme(
#     plot.title = element_text(hjust = 0.5)
#   )
# ggplot(data = ts3,
#        mapping = aes(x = Date,
#                      y = SST)) +  
#   geom_line(col = 'deeppink')  +
#   theme_minimal(base_size=14) +
#   labs(title = 'Sea surface temperature in the Pacific from 2000 to 2026.', 
#        x = 'Time (day)',
#        y = 'Sea surface temperature (C)')+
#   theme(
#     plot.title = element_text(hjust = 0.5)
#   )
# ggplot(data = ts4,
#        mapping = aes(x = Date,
#                      y = SST)) +  
#   geom_line(col = 'deeppink')  +
#   theme_minimal(base_size=14) +
#   labs(title = 'Sea surface temperature in the Pacific from 2000 to 2026.', 
#        x = 'Time (day)',
#        y = 'Sea surface temperature (C)')+
#   theme(
#     plot.title = element_text(hjust = 0.5)
#   )
# ggplot(data = ts5,
#        mapping = aes(x = Date,
#                      y = SST)) +  
#   geom_line(col = 'deeppink')  +
#   theme_minimal(base_size=14) +
#   labs(title = 'Sea surface temperature in the Pacific from 2000 to 2026.', 
#        x = 'Time (day)',
#        y = 'Sea surface temperature (C)')+
#   theme(
#     plot.title = element_text(hjust = 0.5)
#   )
# ggplot(data = ts6,
#        mapping = aes(x = Date,
#                      y = SST)) +  
#   geom_line(col = 'deeppink')  +
#   theme_minimal(base_size=14) +
#   labs(title = 'Sea surface temperature in the Pacific from 2000 to 2026.', 
#        x = 'Time (day)',
#        y = 'Sea surface temperature (C)')+
#   theme(
#     plot.title = element_text(hjust = 0.5)
#   )
# ggplot(data = ts7,
#        mapping = aes(x = Date,
#                      y = SST)) +  
#   geom_line(col = 'deeppink')  +
#   theme_minimal(base_size=14) +
#   labs(title = 'Sea surface temperature in the Pacific from 2000 to 2026.', 
#        x = 'Time (day)',
#        y = 'Sea surface temperature (C)')+
#   theme(
#     plot.title = element_text(hjust = 0.5)
#   )

library(dplyr)
library(tsibble)
library(purrr)

all_ts = list(ts1,ts2,ts3,ts4,ts5,ts6,ts7) |>
  map_dfr(as_tibble,.id='station_id') |>
  group_by(Date) |>
  summarise(
    mean_SST   = if (all(is.na(SST))) NA_real_ else mean(SST, na.rm = TRUE),
    min_SST    = if (all(is.na(SST))) NA_real_ else min(SST, na.rm = TRUE),
    max_SST    = if (all(is.na(SST))) NA_real_ else max(SST, na.rm = TRUE),
    active_obs = sum(!is.na(SST)),
    .groups    = "drop"
  ) |> 
  as_tsibble(index = Date)

# Plot:
ggplot(data = all_ts,
       mapping = aes(x = Date,
                     y = mean_SST)) +
  geom_line(col = 'deeppink')  +
  theme_minimal(base_size=14) +
  labs(title = 'Sea surface temperature in the Pacific from 2000 to 2026.',
       x = 'Time (day)',
       y = 'Sea surface temperature (C)')+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# View(all_ts)

# Truncate time series data:
cut_ts = all_ts[1:7974,]

# Plot:
ggplot(data = cut_ts,
       mapping = aes(x = Date,
                     y = mean_SST)) +
  geom_line(col = 'deeppink')  +
  theme_minimal(base_size=14) +
  labs(title = 'Sea surface temperature in the Pacific from 2000 to 2026.',
       x = 'Time (day)',
       y = 'Sea surface temperature (C)')+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Aggregate to monthly values:
monthly_ts = cut_ts |>
  index_by(year_month=~yearmonth(.))|>
  summarise(current_mean=mean(mean_SST,na.rm=TRUE))

# Plot:
ggplot(data = monthly_ts,
       mapping = aes(x = year_month,
                     y = current_mean)) +  
  geom_line(col = 'deeppink')  +
  theme_minimal(base_size=14) +
  labs(title = 'Sea surface temperature in the Pacific from 2000 to 2026.',
       x = 'Time (day)',
       y = 'Sea surface temperature (C)')+
  theme(
    plot.title = element_text(hjust = 0.5)
  )

# Season: 
season_plot = monthly_ts |> 
  ggtime::gg_season(current_mean, 
                    labels = "right") +
  labs(title = 'Seasonal pattern from 2000 to 2021.', 
       x = 'Month',
       y = 'Average sea surface temperature (C)') +
  theme_minimal(base_size=14)+
  theme(
    plot.title = element_text(hjust = 0.5)
  )
season_plot

# Subseries:
subseries_plot = monthly_ts |>
  ggtime::gg_subseries(current_mean) +
  labs(title = 'Monthly trends from 2000 to 2021.', 
       x = '',
       y = 'Average sea surface temperature (C)') +
  theme_minimal(base_size=14) + 
  theme(
    axis.text.x = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
subseries_plot

# Lag:
lag_plot = monthly_ts |> 
  ggtime::gg_lag(current_mean, 
                 geom = 'point', 
                 lags = 1:12) +
  labs(title = 'Lag plots',
       subtitle = 'Lags 1 to 12.',
       x = 'Lag',
       y = 'Average sea surface temperature (C)') +
  theme_minimal(base_size=14)+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust=0.5)
  )
lag_plot

# Autocorrelation Function:
acf_plot = monthly_ts |>
  ACF(current_mean, 
      lag_max = 36) |> 
  ggtime::autoplot() +
  labs(title = 'ACF plot for a 3-year horizon.') +
  theme_minimal(base_size=14)+
  theme(
    plot.title = element_text(hjust = 0.5)
  )
acf_plot

# Partial Autocorrelation Function:
pacf_plot = monthly_ts |>
  PACF(current_mean, 
       lag_max = 36) |> 
  ggtime::autoplot() +
  labs(title = "PACF plot for a 3-year horizon.") +
  theme_minimal(base_size=14)+
  theme(
    plot.title = element_text(hjust = 0.5)
  )
pacf_plot

# Fit STL model:
library(fable)
library(imputeTS)

monthly_ts_clean = monthly_ts |>
  fill_gaps() |>
  mutate(current_mean = na_kalman(current_mean)) # is this okay? 

stl_fit = monthly_ts_clean |> 
  model(STL(current_mean ~ trend(window = 21) + 
              season(window = 'periodic')))

# Extract components and plot:
stl_plot = stl_fit |> 
  components() |> 
  ggtime::autoplot(colour='purple') +
  labs(title = 'STL decomposition of sea surface temperature.',
       subtitle = 'Average sea surface temperature = trend + seasonality + noise.',
       x = 'Year') +
  theme_minimal(base_size=14)+
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust=0.5)
  )
stl_plot


# View(monthly_ts)
