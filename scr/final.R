library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(stringr)
library(ggplot2)
library(hms)

#Read CSV
raw <- read_csv("data/ethanol2_3_5Bleach50_100_200ppm_PQ50_100_150uM_D74D105_ex3.csv")

#Growth Curve for A909, A909 D74, A909 D105 Treated with Bleach with concenration of untreated, 50ppm, 100ppm, 200ppm with 3 replicate.
#Read + extract
time_row <- 26
dat <- raw[(time_row +1):nrow(raw), 2:39]
colnames(dat) <- as.character(raw[time_row, 2:39])
dat <- dat[complete.cases(dat[[1]]),]



#Convert wells to numeric
colnames(dat)[1:38] <- c("time", "Temperature","A1","A2","A3","A4","A5","A6","A7","A8","A9","A10","A11","A12","B1","B2","B3","B4","B5","B6","B7","B8","B9","B10","B11","B12","C1","C2","C3","C4","C5","C6","C7","C8","C9","C10","C11","C12")
well_cols <- 3:ncol(dat)
dat[well_cols] <- lapply(dat[well_cols], function (x) as.numeric(as.character(x)))

#Convert the data into Long format
dat_long <- dat %>%
  pivot_longer(
    cols = 3:ncol(dat),   # all well columns
    names_to = "Well",
    values_to = "OD"
  )

#Designate the Conditions and Replicates
dat_long <- dat_long %>%
  mutate(
    Condition = case_when(
      Well %in% c("A1","A2","A3") ~ "A909_un",
      Well %in% c("A4","A5","A6") ~ "A909_50ppm",
      Well %in% c("A7","A8","A9") ~ "A909_100ppm",
      Well %in% c("A10","A11","A12") ~ "A909_200ppm",
      Well %in% c("B1","B2","B3") ~ "A909_D75_un",
      Well %in% c("B4","B5","B6") ~ "A909_D75_50ppm",
      Well %in% c("B7","B8","B9") ~ "A909_D75_100ppm",
      Well %in% c("B10","B11","B12") ~ "A909_D75_200ppm",
      Well %in% c("C1","C2","C3") ~ "A909_D105_un",
      Well %in% c("C4","C5","C6") ~ "A909_D105_50ppm",
      Well %in% c("C7","C8","C9") ~ "A909_D105_100ppm",
      Well %in% c("C10","C11","C12") ~ "A909_D105_200",
      TRUE ~ NA_character_
    ),
    Replicate = case_when(
      Well %in% c("A1","A4","A7","A10") ~ "R1",
      Well %in% c("A2","A5","A8","A11") ~ "R2",
      Well %in% c("A3","A6","A9","A12") ~ "R3",
      Well %in% c("B1","B4","B7","B10") ~ "R4",
      Well %in% c("B2","B5","B8","B11") ~ "R5",
      Well %in% c("B3","B6","B9","B12") ~ "R6",
      Well %in% c("C1","C4","C7","C10") ~ "R7",
      Well %in% c("C2","C5","C8","C11") ~ "R8",
      Well %in% c("C3","C6","C9","C12") ~ "R9",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Condition))


#Convert the time into hours

dat_long <- dat_long |>
  mutate(
    time_hours = as.numeric(hms::as_hms(time)) / 3600
  )
dat_long <- dat_long |>
  filter(!is.na(time_hours))

#Data summaries

summary_data <- dat_long %>%
  group_by(time_hours, Condition) %>%
  summarise(
    mean_OD = mean(OD, na.rm = TRUE),
    sd_OD   = sd(OD, na.rm = TRUE),
    n       = sum(!is.na(OD)),
    se_OD   = sd_OD / sqrt(n),
    .groups = "drop"
  ) %>%
  filter(!is.na(time_hours))
#VISUALIZATIONS

# Plot 1: Growth curves of A909 in Bleach Treatment

ggplot(summary_data, aes(x = time_hours, y = mean_OD, color = Condition)) +
  geom_line(size = 1.2) +
  geom_ribbon(aes(ymin = mean_OD - se_OD, ymax = mean_OD + se_OD, fill = Condition),
              alpha = 0.2, color = NA) +
  theme_classic() +
  labs(
    title = "GBS Growth Curve",
    x = "Time (hours)",
    y = "OD600"
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