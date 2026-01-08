# ==============================================================================
# Script: 02_Process_XACT_Metals.R
# Purpose: Process XACT data, calc Coarse fraction, filter by high PM events.
# ==============================================================================

# 1. Load Packages --------------------------------------------------------
library(dplyr)
library(lubridate)
library(data.table)
library(tidyr)

# 2. Helper Function: Load & Clean XACT Data ------------------------------
process_xact <- function(filepath, pollutant_prefix) {
  
  # Check if file exists
  if (!file.exists(filepath)) stop(paste("Error: File not found:", filepath))
  
  # Load CSV
  df <- read.csv(filepath, row.names = NULL)
  
  # Fix Date
  df$datetime <- as_datetime(df$datetime)
  
  # Select and Rename Columns (Standardizing names)
  # Expected format: "PM10_Al.13..ug.m3." -> "Al"
  
  metal_cols <- c("datetime",
                  paste0(pollutant_prefix, "_Ag.47..ug.m3."), paste0(pollutant_prefix, "_Al.13..ug.m3."), 
                  paste0(pollutant_prefix, "_As.33..ug.m3."), paste0(pollutant_prefix, "_Ba.56..ug.m3."), 
                  paste0(pollutant_prefix, "_Br.35..ug.m3."), paste0(pollutant_prefix, "_Ca.20..ug.m3."), 
                  paste0(pollutant_prefix, "_Cl.17..ug.m3."), paste0(pollutant_prefix, "_Cr.24..ug.m3."), 
                  paste0(pollutant_prefix, "_Cu.29..ug.m3."), paste0(pollutant_prefix, "_Fe.26..ug.m3."), 
                  paste0(pollutant_prefix, "_K.19..ug.m3."),  paste0(pollutant_prefix, "_Mn.25..ug.m3."), 
                  paste0(pollutant_prefix, "_Ni.28..ug.m3."), paste0(pollutant_prefix, "_Pb.82..ug.m3."), 
                  paste0(pollutant_prefix, "_Pd.46..ug.m3."), paste0(pollutant_prefix, "_Sb.51..ug.m3."), 
                  paste0(pollutant_prefix, "_Se.34..ug.m3."), paste0(pollutant_prefix, "_Si.14..ug.m3."), 
                  paste0(pollutant_prefix, "_Sr.38..ug.m3."), paste0(pollutant_prefix, "_Te.52..ug.m3."), 
                  paste0(pollutant_prefix, "_Ti.22..ug.m3."), paste0(pollutant_prefix, "_V.23..ug.m3."), 
                  paste0(pollutant_prefix, "_Zn.30..ug.m3."))
  
  new_names <- c('date', 'Ag', 'Al', 'As', 'Ba', 'Br', 'Ca', 'Cl', 'Cr', 'Cu', 
                 'Fe', 'K', 'Mn', 'Ni', 'Pb', 'Pd', 'Sb', 'Se', 'Si', 'Sr', 
                 'Te', 'Ti', 'V', 'Zn')
  
  # Subset and Rename
  df_clean <- df[, metal_cols]
  colnames(df_clean) <- new_names
  
  # Filter missing data (using Al as indicator)
  df_clean <- df_clean %>% filter(!is.na(Al))
  
  # Aggregate to 3-Hour Means
  # Group by ceiling_date (3 hours) and take mean of all numeric columns
  df_3hr <- df_clean %>%
    group_by(date = ceiling_date(date, '3 hour')) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE), .groups = 'drop') %>%
    as.data.frame()
  
  return(df_3hr)
}

# 3. Load & Process PM10 and PM2.5 ----------------------------------------
message("Loading XACT Data...")

# Define relative paths (Assuming flat structure in 'data/' folder)
path_xact_pm10 <- "data/XACT_201907to202303_pm10.csv"
path_xact_pm25 <- "data/XACT_202101to202303_pm25.csv"

# Process
PM10_3hr  <- process_xact(path_xact_pm10, "PM10")
PM25_3hr  <- process_xact(path_xact_pm25, "PM2.5")

# 4. Calculate Coarse Fraction (PM10 - PM2.5) -----------------------------
message("Calculating Coarse Fraction...")

# Merge PM10 and PM2.5 by date
# Suffixes .10 and .25 will be added to distinguish columns
combined <- inner_join(PM10_3hr, PM25_3hr, by = "date", suffix = c(".10", ".25"))

# List of elements
elements <- c('Ag', 'Al', 'As', 'Ba', 'Br', 'Ca', 'Cl', 'Cr', 'Cu', 
              'Fe', 'K', 'Mn', 'Ni', 'Pb', 'Pd', 'Sb', 'Se', 'Si', 
              'Sr', 'Te', 'Ti', 'V', 'Zn')

# Calculate Coarse = PM10 - PM2.5 for each element
XACT_Coarse <- combined %>% select(date) # Start with just date

for(el in elements) {
  col_10 <- paste0(el, ".10")
  col_25 <- paste0(el, ".25")
  
  # Calculate diff
  XACT_Coarse[[el]] <- combined[[col_10]] - combined[[col_25]]
}

# 5. Filter for High-Concentration Events ---------------------------------
message("Filtering for High-Concentration Events...")

# Load the 99th percentile events from Script 01
if(!file.exists("data/processed/01_High_Concentration_Events.rds")) {
  stop("Error: 01_High_Concentration_Events.rds not found. Run Script 01 first.")
}
Events_99 <- readRDS("data/processed/01_High_Concentration_Events.rds")

# Merge: This keeps ONLY the dates that are in BOTH datasets
# (i.e., High PMcoarse events that also have valid XACT data)
Final_Dataset <- inner_join(Events_99[, c("date", "mean_concentration.PM_coarse")], 
                            XACT_Coarse, by = "date")

# Rename the PMcoarse column for clarity
colnames(Final_Dataset)[2] <- "PMcoarse"

# 6. Save Processed Data --------------------------------------------------
saveRDS(Final_Dataset, "data/processed/02_XACT_Coarse_Events.rds")

message("Script 02 Complete. Data saved to data/processed/02_XACT_Coarse_Events.rds")
message(paste("Final Number of Events identified:", nrow(Final_Dataset)))