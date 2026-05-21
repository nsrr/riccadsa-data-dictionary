version <- "0.1.0.pre1"
#releasepath <- "/Volumes/bwh-sleepepi-nsrr-staging/"

library(tidyverse)
library(haven)

full_dict <- read.csv("/Users/rh306/full_dict_1.csv")
cpap_vars <- read.delim("/Users/rh306/cpap_vars.txt", header = F)
delete_vars <- read.delim("/Users/rh306/Library/CloudStorage/OneDrive-MassGeneralBrigham/git/RICCADSA-DD-prep/vars_deleted.txt", header = F)
data_path <- '/Users/rh306/Partners HealthCare Dropbox/Runpeng Hu/nsrr-riccadsa/511_RICCADSA_ForNSRR_Updated_03April2026.sav'
release_path <- "/Users/rh306/Library/CloudStorage/OneDrive-MassGeneralBrigham/git/RICCADSA-DD-prep"

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
  arrange(patnr, timepoint)


write.csv(main_df, file.path(release_path, paste0("riccadsa-dataset-", version, ".csv")), na = "", row.names = F)

cpap_df <- df_long |>
  select(c(patnr, riccadsa_id, timepoint, all_of(cpap_vars$V1)))

write.csv(cpap_df, file.path(release_path, paste0("/riccadsa-cpap-dataset-", version, ".csv")), na = "", row.names = F)

         
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
    nsrr_ahi_ = psg_ahi,
    nsrr_odi_dsge4 = psg_odi,
    nsrr_pctdursp_s3 = psg_delta_percent,
    nsrr_pctdursp_sr = psg_rem_percent,
    nsrr_bp_diastolic = dbp,
    nsrr_bp_systolic = sbp, #sbp baseline
    nsrr_phrnumar_f1 = psg_arousal_total#Arousal Index: Number of arousals per hour of sleep from polysomnography
  ) |>
  arrange(nsrrid, nsrr_visit)

write.csv(df_h, file.path(release_path, paste0("/riccadsa-harmonized-dataset-", version, ".csv")), na = "", row.names = F)
