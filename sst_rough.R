library(dplyr)
library(stringr)

# 1. Read all raw text lines from the file
raw_lines <- readLines("projects/_data/sst10s10w_dy.ascii")

# 2. Extract only lines starting with an 8-digit date (e.g., "20170309 ...")
data_lines <- raw_lines[grepl("^[0-9]{8}\\s", str_trim(raw_lines))]

# 3. Read the cleaned lines directly into a data frame
df <- read.table(text = data_lines, header = FALSE)
colnames(df) <- c("YYYYMMDD", "HHMM", "SST", "Q", "S")

# 4. Clean up missing values (-9.99) and convert date formats
df_clean <- df %>%
  filter(SST != -9.99) %>%
  mutate(Date = as.Date(as.character(YYYYMMDD), format = "%Y%m%d"))

# View the final result
head(df_clean)

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
