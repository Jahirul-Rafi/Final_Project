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
dat <- raw[(time_row +1):nrow(raw), 2:15]
colnames(dat) <- as.character(raw[time_row, 2:15])
dat <- dat[complete.cases(dat[[1]]),]



#Convert wells to numeric
colnames(dat)[1:14] <- c("time", "Temperature","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12")
well_cols <- 3:ncol(dat)
dat[well_cols] <- lapply(dat[well_cols], function (x) as.numeric(as.character(x)))

dat_long <- dat %>%
  pivot_longer(
    cols = 3:ncol(dat),   # all well columns
    names_to = "Well",
    values_to = "OD"
  )

dat_long <- dat_long %>%
  mutate(
    Condition = case_when(
      Well %in% c("A1","A2","A3") ~ "A909_un",
      Well %in% c("A4","A5","A6") ~ "A909_50ppm",
      Well %in% c("A7","A8","A9") ~ "A909_100ppm",
      Well %in% c("A10","A11","A12") ~ "A909_200ppm",
      TRUE ~ NA_character_
    ),
    Replicate = case_when(
      Well %in% c("A1","A4","A7","A10") ~ "R1",
      Well %in% c("A2","A5","A8","A11") ~ "R2",
      Well %in% c("A3","A6","A9","A12") ~ "R3",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Condition))


#Pivot long
long_data <- dat|>
  pivot_longer(
    cols = -c("time", "Temperature"),
    names_to = "well",
    values_to = "absorbance",
    values_drop_na = TRUE
  ) %>%
  mutate(
    time_hours = as.numeric(substr(time, 1, regexpr(":", time) -1)),
    absorbance = as.numeric(absorbance),
    bleach = as.numeric(bleach)
  ) %>%
  filter(!is.na(absorbance) & absorbance > 0)



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