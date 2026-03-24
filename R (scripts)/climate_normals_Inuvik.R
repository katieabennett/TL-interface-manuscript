#
#Climate normals code 
#authors: Oliver Sonnentag and Kathryn Bennett
#Create Figure S2 and Table S4
#--------------------------------------------------
# Install and load required packages
# --------------------------------------------------
if (!require("weathercan")) {
  install.packages("weathercan", repos = c("https://ropensci.r-universe.dev", "https://cloud.r-project.org"))
}
if (!require("dplyr")) install.packages("dplyr")
library(weathercan)
library(dplyr)

# Define WMO IDs for Inuvik Climate and Inuvik UA
# --------------------------------------------------
station_primary <- 41883  # Inuvik Climate
station_secondary <- 8938  # Inuvik UA
# --------------------------------------------------
# Download daily data from 1991 to 2020
# --------------------------------------------------
climate_data_primary <- weather_dl(
  station_ids = station_primary,
  start = "1991-01-01",
  end = "2023-12-31",
  interval = "day"
)
climate_data_secondary <- weather_dl(
  station_ids = station_secondary,
  start = "1991-01-01",
  end = "2023-12-31",
  interval = "day"
)
# --------------------------------------------------
# Combine and gap-fill: primary station filled with secondary when missing
# --------------------------------------------------
# Join by date
climate_data <- full_join(
  climate_data_primary %>% select(date, mean_temp, total_precip),
  climate_data_secondary %>% select(date, mean_temp, total_precip),
  by = "date",
  suffix = c("_primary", "_secondary")
) %>%
  mutate(
    mean_temp = ifelse(!is.na(mean_temp_primary), mean_temp_primary, mean_temp_secondary),
    total_precip = ifelse(!is.na(total_precip_primary), total_precip_primary, total_precip_secondary)
  ) %>%
  select(date, mean_temp, total_precip)
# --------------------------------------------------
# Aggregate to annual summaries
# --------------------------------------------------
annual <- climate_data %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(
    mean_tair = mean(mean_temp, na.rm = TRUE),
    total_ppt = sum(total_precip, na.rm = TRUE)
  ) %>%
  ungroup()

# v2 of annual df that calculates sd, min, max, IQR for each year
annual_v2 <- climate_data %>%
  mutate(year = as.integer(format(date, "%Y"))) %>%
  group_by(year) %>%
  summarise(
    mean_tair = mean(mean_temp, na.rm = TRUE),
    sd_tair = sd(mean_temp, na.rm = TRUE),
    min_tair = min(mean_temp, na.rm = TRUE),
    max_tair = max(mean_temp, na.rm = TRUE),
    iqr_tair = IQR(mean_temp, na.rm = TRUE),
    
    total_ppt = sum(total_precip, na.rm = TRUE),
    min_ppt = min(total_precip, na.rm = TRUE),
    max_ppt = max(total_precip, na.rm = TRUE)
  ) %>%
  ungroup()
annual_v2

# --------------------------------------------------
# Compute climate normals and variability (1991–2020)
# --------------------------------------------------
climate_stats <- annual %>%
  filter(year >= 1991, year <= 2023) %>%
  summarise(
    mean_tair_avg = mean(mean_tair, na.rm = TRUE),
    sd_tair = sd(mean_tair, na.rm = TRUE),
    min_tair = min(mean_tair, na.rm = TRUE),
    max_tair = max(mean_tair, na.rm = TRUE),
    iqr_tair = IQR(mean_tair, na.rm = TRUE),
    
    mean_ppt_avg = mean(total_ppt, na.rm = TRUE),
    sd_ppt = sd(total_ppt, na.rm = TRUE),
    min_ppt = min(total_ppt, na.rm = TRUE),
    max_ppt = max(total_ppt, na.rm = TRUE),
    iqr_ppt = IQR(total_ppt, na.rm = TRUE)
  )
print(climate_stats)
# --------------------------------------------------
# Identify anomalies and extremes for year period e.g.,2022–2024
# Compares the elected years to values from the full period in annual (e.g., 1990-2024)
# --------------------------------------------------
tair_q1 <- quantile(annual$mean_tair, 0.25, na.rm = TRUE)
tair_q3 <- quantile(annual$mean_tair, 0.75, na.rm = TRUE)
ppt_q1  <- quantile(annual$total_ppt, 0.25, na.rm = TRUE)
ppt_q3  <- quantile(annual$total_ppt, 0.75, na.rm = TRUE)
anomaly_years <- annual %>%
  filter(year >= 2022, year <= 2023) %>%
  mutate(
    tair_anomaly = case_when(
      mean_tair < tair_q1 ~ "Cool",
      mean_tair > tair_q3 ~ "Warm",
      TRUE ~ "Normal"
    ),
    ppt_anomaly = case_when(
      total_ppt < ppt_q1 ~ "Dry",
      total_ppt > ppt_q3 ~ "Wet",
      TRUE ~ "Normal"
    ),
    tair_extreme = mean_tair < climate_stats$min_tair | mean_tair > climate_stats$max_tair,
    ppt_extreme = total_ppt < climate_stats$min_ppt | total_ppt > climate_stats$max_ppt
  )
print(anomaly_years)


#################################
# plot monthly values for years of interest
# Create Figure S2
################################
library(lubridate)
library(ggplot2)
library(patchwork)

#############################################
#download data from Trail Valley climate station (TVC), note: not Inuvik
station_tvc <- 71683 #Trail Valley

#download daily data for TVC
climate_data_tvc <- weather_dl(
  station_ids = station_primary,
  start = "2022-01-01",
  end = "2023-12-31",
  interval = "day"
)
################################################

#for Inuvik
monthly_climate <- climate_data %>%
  filter(year(date) %in% c(2022, 2023)) %>%
  group_by(year = year(date), month = month(date, label = TRUE)) %>%
  summarise(
    mean_tair = mean(mean_temp, na.rm = TRUE),
    temp_sd = sd(mean_temp, na.rm = TRUE),
    month_precip = sum(total_precip, na.rm = TRUE),
    .groups = "drop"
  )
head(monthly_climate)

#for TVC
tvc_monthly_climate <- climate_data_tvc %>%
  group_by(year = year(date), month = month(date, label = TRUE)) %>%
  summarise(
    mean_tair = mean(mean_temp, na.rm = TRUE),
    temp_sd = sd(mean_temp, na.rm = TRUE),
    month_precip = sum(total_precip, na.rm = TRUE),
    .groups = "drop"
  )
head(tvc_monthly_climate)

#plot
p_precip <- ggplot(tvc_monthly_climate,
       aes(x = month,
           y = month_precip,
           fill = factor(year))) +
  geom_col(position = "dodge") +
  scale_fill_brewer(
    palette = "Set1",
    name = "Year"
  ) +
  labs(
    y = "Total precipitation (mm)",
    x = "Month"
  ) +
  theme_classic()+
  theme(legend.position = "bottom")


p_temp <- ggplot(tvc_monthly_climate,
       aes(x = month,
           y = mean_tair,
           color = factor(year),
           group = year)) +
  geom_point(size = 3, position = position_dodge(width = 0.3)) +
  geom_errorbar(
    aes(ymin = mean_tair - temp_sd,
        ymax = mean_tair + temp_sd),
    width = 0.15,
    position = position_dodge(width = 0.3)
  ) +
  scale_color_brewer(
    palette = "Set1",
    name = "Year"
  ) +
  geom_line(position = position_dodge(width = 0.3))+
  labs(
    x = "",
    y = "Mean air temperature (°C)",
  ) +
  theme_classic()+
  theme(legend.position = "none")

combined_plot <- p_temp/p_precip+
  plot_annotation(tag_levels = "a") &
  theme(plot.tag.position = c(0, 1),
        plot.tag = element_text(size = 16, 
                                hjust = 1, vjust = 0))
combined_plot

#export plot
ggsave(combined_plot, 
       filename = "plots/monthly_climate.png", 
       device = "png",
       height = 5, width = 6.5, units = "in") 



