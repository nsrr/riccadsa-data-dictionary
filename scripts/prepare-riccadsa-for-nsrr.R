version <- "0.1.0"
setwd("/Volumes/bwh-sleepepi-nsrr-staging/20260521-riccadsa")


library(tidyverse)
library(haven)


full_dict <- read.csv("nsrr-prep/metadata/full_dict_map.csv") # file for mapping repeated measures

cpap_vars <- read.delim("nsrr-prep/metadata/cpap_vars.txt", header = F) # list of cpap variables to separate into the cpap dataset
delete_vars <- read.delim("nsrr-prep/metadata/vars_deleted.txt", header = F) #list of variables to be deleted from the posted dataset (redundant varaibles, or irrelevant variables, as agreed on by NSRR and RICCADSA team)

data_path <- 'original/511_RICCADSA_ForNSRR_Updated_03April2026.sav'

release_dir <- file.path("nsrr-prep", "_releases", version)

if (!dir.exists(release_dir)) {
  dir.create(release_dir, recursive = TRUE)
}


df <- read_sav(data_path)

df <- df |>
  select(-all_of(delete_vars$V1))|>
  rename_with(tolower)

date_vars <- c(
  "date",
  "date_inter",
  "date_screening",
  "echo_date",
  "final_date",
  "fu",
  "ami_date",
  "cvd_mort_date",
  "date_hosp_af_cardiac_failure",
  "firstevent_date",
  "incident_stroke_date",
  "mortdate",
  "newcabg_date",
  "newpci_date",
  "newrevasc_date",
  "exercise_date",
  "date_blood",
  "cpapreturndate",
  "cpap_ctrl_date",
  "cpapstartdate",
  "date_psg"
)

character_vars <- c(
  "cause_of_death",
  "smokinghistory_comments",
  "reason_english",
  "timepoint"
)

id_vars <- c(
  "patnr",
  "riccadsa_id")

df_long <- df |>
  pivot_longer(
    cols = -any_of(id_vars),
    names_to = "original_var",
    values_to = "value",
    values_transform = list(value = as.character)
  ) |>
  left_join(full_dict, by = "original_var") |>
  select(patnr, riccadsa_id, timepoint, measure, value) |>
  pivot_wider(
    names_from = measure,
    values_from = value ) |>
  mutate(
    across(any_of(date_vars), as.Date),
    across(any_of(character_vars), as.character),
    across(
      -any_of(c(id_vars, date_vars, character_vars)),
      ~ parse_number(as.character(.x))
    )
  )

timepoints_main <- c(
  "V0_screening",
  "V1_baseline",
  "V2_3m",
  "V3_1yr",
  "outcome"
)


### Further de-identify by removing all the dates: 

# 1. Pull screening date for each participant
screening_dates <- df_long |>
  filter(timepoint == "V0_screening") |>
  select(patnr, riccadsa_id, date_screening) |>
  mutate(date_screening = as.Date(date_screening))

# 2. Join screening date back to all rows
df_long2 <- df_long |>
  select(-date_screening) |>
  left_join(screening_dates, by = c("patnr", "riccadsa_id")) |>
  mutate(daysfrom_interv = coalesce(daysto_screening, daysfrom_interv),
         daysto_visit = as.numeric(date - date_screening),
    daysto_echo = if_else(
      is.na(daysto_echo),
      as.numeric(echo_date - date_screening),
      daysto_echo
    ),
    daysto_final = if_else(
      timepoint == "outcome",
      as.numeric(final_date - date_screening),
      NA_real_
    ),
    daysto_ami = if_else(
      timepoint == "outcome",
      as.numeric(ami_date - date_screening),
      NA_real_
    ),
    daysto_cvd_mort = if_else(
      timepoint == "outcome",
      as.numeric(cvd_mort_date - date_screening),
      NA_real_
    ),
    daysto_hosp = if_else(
      timepoint == "outcome",
      as.numeric(date_hosp_af_cardiac_failure - date_screening),
      NA_real_
    ),
    daysto_firstevent = if_else(
      timepoint == "outcome",
      as.numeric(firstevent_date - date_screening),
      NA_real_
    ),
    daysto_stroke = if_else(
      timepoint == "outcome",
      as.numeric(incident_stroke_date - date_screening),
      NA_real_
    ),
    daysto_mort = if_else(
      timepoint == "outcome",
      as.numeric(mortdate - date_screening),
      NA_real_
    ),
    daysto_cabg = if_else(
      timepoint == "outcome",
      as.numeric(newcabg_date - date_screening),
      NA_real_
    ),
    daysto_newpci = if_else(
      timepoint == "outcome",
      as.numeric(newpci_date - date_screening),
      NA_real_
    ),
    daysto_newrevasc = if_else(
      timepoint == "outcome",
      as.numeric(newrevasc_date - date_screening),
      NA_real_
    ),
    daysto_exercise = if_else(
      !is.na(exercise_date) & !is.na(date_screening),
      as.numeric(exercise_date - date_screening),
      NA_real_
    )
  ) |>
  relocate(daysfrom_interv, .after = date_inter) |>
  relocate(daysto_visit, .after = daysfrom_interv) |>
  relocate(daysto_echo, .after = echo_date) |>
  relocate(daysto_final, .after = final_date) |>
  relocate(daysto_ami, .after = ami_date) |>
  relocate(daysto_cvd_mort, .after = cvd_mort_date) |>
  relocate(daysto_hosp, .after = date_hosp_af_cardiac_failure) |>
  relocate(daysto_firstevent, .after = firstevent_date) |>
  relocate(daysto_stroke, .after = incident_stroke_date) |>
  relocate(daysto_mort, .after = mortdate) |>
  relocate(daysto_cabg, .after = newcabg_date) |>
  relocate(daysto_newpci, .after = newpci_date) |>
  relocate(daysto_newrevasc, .after = newrevasc_date) |>
  relocate(daysto_exercise, .after = exercise_date)
###


main_df <- df_long2 |> 
  select(-all_of(c(cpap_vars$V1, date_vars))) |>
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
      patnr == "647" & ssri == 53 ~ NA_real_,
      TRUE ~ ssri
    ),
    hip = case_when(
      patnr == "673" & hip == 12 ~ NA_real_,
      TRUE ~ hip
    ),
    whr = case_when(
      patnr == "673" ~ NA_real_,
      TRUE ~ whr
    ),
    l = case_when(
      patnr == "684" & l == 83   ~ 183,
      patnr == "424" & l == 90   ~ 169,
      patnr == "198" & l == 109  ~ 174,
      TRUE ~ l
    ),
    weight = case_when(
      patnr == "424" & weight == 169 ~ 90,
      patnr == "198" & weight == 174 ~ 109,
      TRUE ~ weight
    ),
    max_bp = case_when(
      patnr == "31" & timepoint == "V3_1yr" ~ NA_real_,
      TRUE ~ max_bp
    ), 
    psg_mean_pulse = case_when(
      patnr == "112" & psg_mean_pulse == 4 ~ NA_real_,
      TRUE ~ psg_mean_pulse
    ),
    psg_av_oxygensat_rem = case_when(
      psg_av_oxygensat_rem == 0 ~ NA_real_,
      TRUE ~ psg_av_oxygensat_rem
    ),
    psg_delta_percent = case_when(
      patnr == "225" & psg_delta_percent == 116.3 ~ round(476.5/48, 1), 
      TRUE ~ psg_delta_percent
    )
  ) |>
  rename(visit = timepoint) |>
  select(-daysto_screening)


write.csv(main_df, file.path(release_dir, paste0("/riccadsa-dataset-", version, ".csv")), na = "", row.names = F)

timepoints_cpap <- c(
  "cpap_baseline",
  "1m_cpap",
  "3m_cpap",
  "6m_cpap",
  "1yr_cpap",
  "2yr_cpap",
  "3yr_cpap",
  "4yr_cpap",
  "5yr_cpap",
  "6yr_cpap",
  "cpap_outcome")

cpap_df <- df_long |>
  select(c(patnr, riccadsa_id, timepoint, all_of(cpap_vars$V1)))|>
  filter(timepoint %in% timepoints_cpap) |>
  mutate( # remove undefined values
    mask = case_when(
      patnr == "97" & mask == 0 ~ NA,
      TRUE ~ mask),
    hum = case_when(
      hum == 2 ~ NA,
      TRUE ~ hum)) |>
  mutate(
    timepoint = factor(
      timepoint,
      levels = timepoints_cpap,
      ordered = TRUE
    )) |>
  arrange(patnr, timepoint) |>
  rename(visit = timepoint) |>
  select(-fu) |>
  filter(
    if_any(
      -c(patnr, riccadsa_id, visit),
      ~ !is.na(.)
    )
  ) |>
  relocate(cpap_ctrl_date, .after = cpapstartdate) |>
  select(-c(cpapstartdate, cpapreturndate, cpap_ctrl_date)) |>
  rename(days_cpap_use = cpapd,
         days_start_fu = days)


corrections <- tribble(
  ~df, ~patnr, ~variable, ~old_value, ~new_value, ~reason,
  "main_df", 647, "ssri", 53, NA, "Removed to NA, Undefined code in dictionary",
  "main_df", 673, "hip", 12, NA, "Removed to NA, extreme outlier",
  "main_df", 673, "whr", 8.1667, NA, "Removed to NA, extreme outlier",
  "main_df", 684, "l", 83, 183, "Fix height",
  "main_df", 424, "l", 90, 169, "switch height and weight",
  "main_df", 198, "l", 109, 174, "switch height and weight",
  "main_df", 424, "weight", 169, 90, "switch weight and height",
  "main_df", 198, "weight", 109, 174, "switch weight and height",
  "main_df", 31, "max_bp", 31, NA, "Remove to NA, outlier",
  "main_df", 112, "psg_mean_pulse", 4, NA, "Remove to NA, outlier",
  "main_df", 533, "psg_av_oxygensat_rem", 0, NA, "Remove to NA, outlier",
  "main_df", 403, "psg_av_oxygensat_rem", 0, NA, "Remove to NA, outlier",
  "main_df", 225, "psg_delta_percent", 116.3, 9.9, "Fix delta percent according to delta time and tst",
  "cpap_df", 97, "mask", 0, NA, "Removed to NA, undefined code in dictionary",
  "cpap_df", 233, "hum", 2, NA, "Removed to NA, undefined code in dictionary",
  "cpap_df", 489, "hum", 2, NA, "Removed to NA, undefined code in dictionary",
)


write.csv(cpap_df, file.path(release_dir, paste0("/riccadsa-cpap-dataset-", version, ".csv")), na = "", row.names = F)

id_links <- read.csv("nsrr-prep/metadata/RICCADSA_Links_Basics.csv") |>
  select(PSG_CODES, PATNR) |>
  rename(nsrr_file_prefix = PSG_CODES)
         
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
  arrange(nsrrid, nsrr_visit) |>
  left_join(
    id_links |>
      mutate(visit = "V1_baseline") |>
      select(PATNR, visit, nsrr_file_prefix),
    by = c(
      "nsrrid" = "PATNR",
      "visit" = "visit"
    )
  )

write.csv(df_h, file.path(release_dir, paste0("/riccadsa-harmonized-dataset-", version, ".csv")), na = "", row.names = F)

#checks <-main_df|>select(riccadsa_id, age, visit, l, weight, bmi, waist, hip, whr, max_bp, psg_av_oxygensat_rem,psg_tst ,psg_delta_minutes, psg_delta_percent, psg_mean_pulse)
