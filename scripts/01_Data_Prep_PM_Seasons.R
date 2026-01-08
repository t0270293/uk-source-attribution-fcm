# ==============================================================================
# Script: 01_Data_Prep_PM_Seasons.R
# Purpose: Load PM data, split by season, identify 99th percentile events.
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(dplyr)
library(lubridate)
library(chron)
library(reshape2)
library(ggplot2)
library(tidyr)

# 2. Setup Time Sequence --------------------------------------------------
# Set complete day time to ensure no gaps
ts <- seq.POSIXt(as.POSIXlt("2019-01-01 00:00:00", tz = "Europe/London"), 
                 as.POSIXlt("2023-04-01 00:00:00", tz = "Europe/London"), by="min")
ts <- format.POSIXct(ts, '%Y-%m-%d %H:%M:%S', tz = "Europe/London")
df <- data.frame(date = ts)
df$date <- as_datetime(df$date)
df <- unique(df)

# 3. Load PM Data ---------------------------------------------------------
# Uses relative paths. Assumes files are located directly in 'data/' folder.

# Check if data folder exists
if (!dir.exists("data")) stop("Error: 'data' folder not found.")

PM_data_01 <- read.csv("data/PM_201907to202012.csv", row.names=NULL)
PM_data_02 <- read.csv("data/PM_202101to202212.csv", row.names=NULL)
PM_data_03 <- read.csv("data/PM_202301to202303.csv", row.names=NULL)

PM_data <- rbind(PM_data_01, PM_data_02, PM_data_03)
PM_data$date = as_datetime(PM_data$date)

# Merge with full time sequence to fill gaps
PM_data <- merge(df, PM_data, by ="date", all=TRUE) %>% arrange(date)

# 4. Time Formatting ------------------------------------------------------
PM_data$start_time <- PM_data$date
PM_data$Date <- as.Date(format(PM_data$date, format = "%Y-%m-%d"))
PM_data$Time_start <- format(PM_data$date, format = "%H:%M:%S")
PM_data$Time_start <- times(PM_data$Time_start)

# Helper function to shift time
shift <- function(x, n){
  c(x[-(seq(n))], rep(NA, n))
}

PM_data$end_time <- shift(PM_data$start_time, 1)
PM_data$Time_end <- shift(PM_data$Time_start, 1)
PM_data$Time_end <- times(PM_data$Time_end)

# Select relevant columns (assuming columns 8-14 are PM species + date)
PMcoarse_data <- PM_data[, c(8:14, 7)]
names(PMcoarse_data)[1] = "date"

# 5. Seasonal Splitting & Thresholding ------------------------------------
PMcoarse_data$month <- month(PMcoarse_data$date)

# Function to process each season
process_season <- function(data, months, season_name) {
  
  # Filter by months
  season_data <- data %>% filter(month %in% months)
  
  # Melt to long format
  season_long <- reshape2::melt(season_data, id.vars="date", measure.vars="PM_coarse", 
                                variable.name="type", value.name="Concentration")
  
  # Aggregate to 3-hour means
  season_3hr <- season_long %>%
    group_by(date=ceiling_date(date, '3 hour'), type) %>%
    summarize(mean_concentration = mean(Concentration, na.rm=TRUE), .groups = 'drop') %>%
    as.data.frame()
  
  # Calculate 99th percentile threshold
  threshold_99 <- quantile(season_3hr$mean_concentration, probs = 0.99, na.rm = TRUE)
  print(paste("99th Percentile for", season_name, ":", threshold_99))
  
  # Reshape to wide
  season_wide <- reshape(season_3hr, idvar = "date", timevar = "type", direction = "wide")
  
  # Filter events above threshold
  events_99 <- season_wide %>% filter(mean_concentration.PM_coarse >= threshold_99)
  
  return(list(all_3hr = season_wide, events_99 = events_99))
}

# --- Run for all seasons ---
res_spring <- process_season(PMcoarse_data, c(3,4,5), "Spring")
res_summer <- process_season(PMcoarse_data, c(6,7,8), "Summer")
res_autumn <- process_season(PMcoarse_data, c(9,10,11), "Autumn")
res_winter <- process_season(PMcoarse_data, c(12,1,2), "Winter")

# 6. Combine Results ------------------------------------------------------

# All data (for general plotting)
PMcoarse_3hr_all <- rbind(res_spring$all_3hr, res_summer$all_3hr, 
                          res_autumn$all_3hr, res_winter$all_3hr)

# High Concentration Events only (for clustering)
PMcoarse_3hr_99th <- rbind(res_spring$events_99, res_summer$events_99, 
                           res_autumn$events_99, res_winter$events_99)

# 7. Visualization --------------------------------------------------------
if(!dir.exists("plots")) dir.create("plots", recursive=TRUE)

plot_all <- ggplot(PMcoarse_3hr_all, aes(x=date, y=mean_concentration.PM_coarse)) +
  geom_line(color="darkgoldenrod3") + 
  ggtitle("3-hr-averaged PMcoarse (2019-2023)") + theme_minimal()

plot_99 <- ggplot(PMcoarse_3hr_99th, aes(x=date, y=mean_concentration.PM_coarse)) +
  geom_point(color="darkgoldenrod3") + 
  ggtitle("3-hr-averaged PMcoarse > 99th Percentile") + theme_minimal()

print(plot_all)
print(plot_99)

ggsave("plots/01_PM_TimeSeries.png", plot_all, width=10, height=6)

# 8. Save Data for Next Script --------------------------------------------
# Create output directory if it doesn't exist
if(!dir.exists("data/processed")) dir.create("data/processed", recursive=TRUE)

saveRDS(PMcoarse_3hr_99th, "data/processed/01_High_Concentration_Events.rds")
message("Script 01 Complete. Data saved to data/processed/01_High_Concentration_Events.rds")