library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(ggplot2)

#Read CSV
raw <- read_csv("data/ethanol2_3_5Bleach50_100_200ppm_PQ50_100_150uM_D74D105_ex3.csv")

#Read + extract
time_row <- 32
dat <- raw[(time_row +1):nrow(raw), 1:100]
colnames(dat) <- as.character(raw[time_row, 1:100])
dat <- dat[complete.cases(dat[[1]]),]

#Convert wells to numeric
colnames(dat)[1:2] <- c("time", "temperature")
well_cols <- 3:ncol(dat)
dat[well_cols] <- lapply(dat[well_cols], function (x) as.numeric(as.character(x)))

#Pivot long
long_data <- dat|>
  pivot_longer(
    cols = -c(time, temperature),
    names_to = "well",
    values_to = "absorbance",
    values_drop_na = TRUE
  ) %>%
  mutate(
    time_hours = as.numeric(substr(time, 1, regexpr(":", time) -1)),
    absorbance = as.numeric(absorbance),
    temperature = as.numeric(temperature)
  ) %>%
  filter(!is.na(absorbance) & absorbance > 0)

# Join
plate_map <- tibble(
  well = c("A1","A2","A3","A4"),
  strain = c("WT","WT","WT","WT"),
  treatment = c("ethanol","bleach","pq","control"),
  concentration = c(2, 50, 100, 0),
  day = c(74, 74, 105, 105)
)

data_joined <- long_data %>%
  left_join(plate_map, by = "well")

# Clean variables
data_clean <- data_joined %>%
  mutate(
    strain = factor(strain),
    treatment = factor(treatment),
    day = factor(day),
    stress_type = case_when(
      treatment %in% c("ethanol", "bleach") ~ "chemical",
      treatment == "pq" ~ "oxidative",
      TRUE ~ "control"
    )
  )

#Data summaries

summary_data <- data_clean %>%
  group_by(strain, treatment, concentration, day, time_hours) %>%
  summarise(
    mean_abs = mean(absorbance, na.rm = TRUE),
    sd_abs = sd(absorbance, na.rm = TRUE),
    .groups = "drop"
  )

#VISUALIZATIONS

# Plot 1: Growth curves
plot1 <- ggplot(summary_data, aes(x = time_hours, y = mean_abs, color = strain)) +
  geom_line() +
  facet_wrap(~ treatment) +
  theme_minimal() +
  labs(
    title = "GBS Growth Under Stress Conditions",
    x = "Time (hours)",
    y = "Mean Optical Density"
  )

# Plot 2: Final OD comparison
final_time <- max(summary_data$time_hours, na.rm = TRUE)

plot2 <- summary_data %>%
  filter(time_hours == final_time) %>%
  ggplot(aes(x = treatment, y = mean_abs, fill = strain)) +
  geom_col(position = "dodge") +
  facet_wrap(~ day) +
  theme_minimal() +
  labs(
    title = "Final Optical Density by Treatment and Day",
    y = "Final OD"
  )

# Plot 3: Desiccation effect
plot3 <- summary_data %>%
  filter(time_hours == final_time) %>%
  ggplot(aes(x = day, y = mean_abs, fill = treatment)) +
  geom_col(position = "dodge") +
  theme_minimal() +
  labs(
    title = "Effect of Desiccation Duration on Survival",
    y = "Final OD"
  )