version <- "0.1.0.pre1"
setwd("/Volumes/bwh-sleepepi-nsrr-staging/20260521-riccadsa")

library(tidyverse)
library(haven)


full_dict <- read.csv("nsrr-prep/_datasets/full_dict_map.csv")
cpap_vars <- read.delim("nsrr-prep/_datasets/cpap_vars.txt", header = F)
delete_vars <- read.delim("nsrr-prep/_datasets/vars_deleted.txt", header = F)
data_path <- 'original/511_RICCADSA_ForNSRR_Updated_03April2026.sav'
release_path <- "nsrr-prep/_releases"

df <- read_sav(data_path)

df <- df |>
  select(-all_of(delete_vars$V1))|>
  rename_with(tolower)

df_long <- df |>
  pivot_longer(
    cols = -any_of(c("patnr", "riccadsa_id")),
    names_to = "original_var",
    values_to = "value",
    values_transform = list(value = as.character)
  ) |>
  left_join(full_dict, by = "original_var") |>
  select(patnr, riccadsa_id, timepoint, measure, value) |>
  pivot_wider(
    names_from = measure,
    values_from = value )

timepoints_main <- c(
  "V0_screening",
  "V1_baseline",
  "V2_3m",
  "V3_1yr",
  "outcome"
)


main_df <- df_long |> 
  select(-all_of(cpap_vars$V1)) |>
  filter(timepoint %in% timepoints_main) |>
  mutate(
    timepoint = factor(
      timepoint,
      levels = timepoints_main,
      ordered = TRUE
    )
  ) |>
  arrange(patnr, timepoint) |>
  mutate(
    ssri = case_when(
      patnr == "647" & ssri == "53" ~ NA,
      TRUE ~ ssri)) |>
    rename(visit = timepoint) 


write.csv(main_df, file.path(release_path, paste0(version,"/riccadsa-dataset-", version, ".csv")), na = "", row.names = F)

cpap_df <- df_long |>
  select(c(patnr, riccadsa_id, timepoint, all_of(cpap_vars$V1)))|>
  mutate( # remove undefined values
    mask = case_when(
      patnr == "97" & mask == 0 ~ NA,
      TRUE ~ mask),
    hum = case_when(
      hum == 2 ~ NA,
      TRUE ~ hum)) |>
  rename(visit = timepoint) 


corrections <- tribble(
  ~df, ~patnr, ~variable, ~old_value, ~new_value, ~reason,
  "main_df", 647, "ssri", 53, NA, "Removed to NA, Undefined code in dictionary",
  "cpap_df", 97, "mask", 0, NA, "Removed to NA, undefined code in dictionary",
  "cpap_df", 233, "hum", 2, NA, "Removed to NA, undefined code in dictionary",
  "cpap_df", 489, "hum", 2, NA, "Removed to NA, undefined code in dictionary",
)


write.csv(cpap_df, file.path(release_path, paste0(version,"/riccadsa-cpap-dataset-", version, ".csv")), na = "", row.names = F)

         
timepoints_3 <- c("V1_baseline","V2_3m","V3_1yr")

df_h <- df_long |> 
  select(patnr, timepoint, age, gender, bmi, smoke, psg_tst, psg_ahi,	psg_odi,
         psg_delta_minutes, psg_delta_percent,	psg_rem_minutes, psg_rem_percent, psg_arousal_total,
         psg_av_oxygensat_rem,	psg_av_oxygensat_nrem,	psg_mean_pulse,	bmi, dbp, sbp, currentsmoker)|> 
  filter(timepoint %in% timepoints_3) |>
  mutate(
    timepoint = factor(
      timepoint,
      levels = timepoints_3,
      ordered = TRUE
    )
  ) |>
  transmute(
    nsrrid = patnr,
    visit = timepoint,
    nsrr_visit = timepoint,
    nsrr_age = round(as.numeric(age), 1),
    nsrr_sex = case_match(
      as.numeric(gender),
      0 ~ "male",
      1 ~ "female",
      .default = if_else(
        timepoint == "V1_baseline",
        "not reported",
        NA_character_
      )
    ),
    nsrr_bmi = round(as.numeric(bmi, 1)),
    nsrr_current_smoker = case_match(
      as.numeric(currentsmoker),
      0 ~ "no",
      1 ~ "yes",
      .default = if_else(
        timepoint == "V1_baseline",
        "not reported",
        NA_character_
      )
    ),
    nsrr_tst_f1 = psg_tst,
    #nsrr_ahi_ = psg_ahi, #no harmonized ahi because it doesn't meet standard definition
    nsrr_odi_dsge4 = psg_odi,
    nsrr_pctdursp_s3 = psg_delta_percent,
    nsrr_pctdursp_sr = psg_rem_percent,
    nsrr_bp_diastolic = dbp,
    nsrr_bp_systolic = sbp, #sbp baseline
    nsrr_phrnumar_f1 = psg_arousal_total#Arousal Index: Number of arousals per hour of sleep from polysomnography
  ) |>
  arrange(nsrrid, nsrr_visit)

write.csv(df_h, file.path(release_path, paste0(version, "/riccadsa-harmonized-dataset-", version, ".csv")), na = "", row.names = F)
