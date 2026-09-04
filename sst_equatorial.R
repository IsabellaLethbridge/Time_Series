library(dplyr)
library(stringr)

# Read raw text lines:
sst_eq_raw_line = readLines("projects/_data/sst0n95w_dy.ascii")

# Extract only lines starting with date:
sst_eq_data_lines = sst_eq_raw_line[grepl("^[0-9]{8}\\s", 
                                           str_trim(sst_eq_raw_line))]

# Read cleaned lines into dataframe:
sst_eq_dat = read.table(text = sst_eq_data_lines, header = FALSE)
colnames(sst_eq_dat) = c("YYYYMMDD", "HHMM", "SST", "Q", "S")

# Clean up missing values:
sst_eq_dat = sst_eq_dat |>
  filter(SST!=-9.99) |>
  mutate(Date=as.Date(as.character(YYYYMMDD),format="%Y%m%d"))




library(zoo)
library(ggplot2)

# 1. Convert to an 'xts' or 'zoo' time series object
sst_ts <- zoo(df_clean$SST, order.by = df_clean$Date)

# --- Option A: Quick Base R Plot ---
plot(sst_ts, 
     main = "Sea Surface Temperature over Time (10°S, 10°W)", 
     xlab = "Date", 
     ylab = "SST (°C)", 
     col = "darkblue")

# --- Option B: Custom ggplot2 Plot (Recommended for better visuals) ---
ggplot(df_clean, aes(x = Date, y = SST)) +
  geom_line(color = "#0077b6", linewidth = 0.5) +
  theme_minimal() +
  labs(
    title = "Sea Surface Temperature (10°S, 10°W)",
    x = "Year",
    y = "Temperature (°C)"
  )
